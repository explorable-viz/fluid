import { Class, __nonNull, absurd, assert, classOf } from "./Core"
import { Ord } from "./Ord"

// An object which can be used as a key in an ES6 map (i.e. one for which equality is ===). In particular
// interned objects are persistent objects. Interface so can be extended by VersionedObject, which it is
// convenient to have as an interface.
export interface PersistentObject {
   // ES6 only allows constructor calls via "new".
   constructor_ (...args: Persistent[]): void
}

// Functions are persistent to support primitives.
export type Persistent = null | PersistentObject | boolean | string | number | Function

// Versioned objects are persistent objects that have state that varies across worlds.
export interface VersionedObject<K extends PersistentObject = PersistentObject> extends PersistentObject {
   // Initialise these at object creation (not enumerable).
   __history: Map<World, ObjectState> // history records only enumerable fields
   __id: K
}

export type Versioned<T> = T & VersionedObject

// A memo key which is sourced externally to the system. (The name "External" exists in the global namespace.)
export class ExternalObject implements PersistentObject {
   public id: number

   constructor_ (id: number) {
      this.id = id
   }

   static make (id: number): ExternalObject {
      return make(ExternalObject, id)
   }
}

// Curried map from constructors and arguments to interned objects; curried because composite keys would 
// require either custom equality, which isn't possible with ES6 maps, or interning, which would essentially
// involve the same memoisation logic.
type InternedObjects = Map<Persistent, PersistentObject | Map<Persistent, Object>> // approximate recursive type
const __internedObjs: InternedObjects = new Map

// For versioned objects the map is not curried but takes an (interned) composite key. TODO: treating the constructor
// as part of the key isn't correct because objects can change class. To match the formalism, we need a notion of 
// "metatype" or kind, so that traces and values are distinguished, but within those "kinds" the class can change.
type VersionedObjects = Map<PersistentObject, PersistentObject>
const __versionedObjs: VersionedObjects = new Map

function lookupArg<T extends PersistentObject> (
   ctr: PersistentClass<T>, 
   m: InternedObjects, 
   args: Persistent[], 
   n: number
): PersistentObject | Map<Persistent, Object> {
   // for memoisation purposes, treat constructor itself as argument -1
   const k: Persistent = n === -1 ? ctr : args[n]
   let v: PersistentObject | Map<Persistent, Object> | undefined = m.get(k)
   if (v === undefined) {
      if (n === args.length - 1) {
         v = new ctr
         v.constructor_(...args)
      } else {
         v = new Map
      }
      m.set(k, v)
   }
   return v
}

export type PersistentClass<T extends PersistentObject = PersistentObject> = new () => T

// Hash-consing (interning) object construction.
export function make<T extends PersistentObject> (ctr: PersistentClass<T>, ...args: Persistent[]): T {
   let v: PersistentObject | Map<Persistent, Object> = lookupArg(ctr, __internedObjs, args, -1)
   for (var n: number = 0; n < args.length; ++n) {
      // since there are more arguments, the last v was a (nested) map
      v = lookupArg(ctr, v as InternedObjects, args, n)
   }
   Object.freeze(v)
   return v as T
}

export function versioned (o: Persistent): o is VersionedObject {
   return o !== null && (__nonNull(o) as any).__id !== undefined
}

export function asVersioned<T extends Persistent> (o: T): Versioned<T> {
   if (versioned(o)) {
      return o
   } else {
      return absurd()
   }
}

export function interned (o: Persistent): boolean {
   return o !== null && !versioned(o)
}

// Unlikely to be either performant or entirely sound. Want to emulate the post-state of new ctr. Probably need to
// worry about how this works with inherited properties.
function reclassify (o: Object, ctr: Class<Object>): void {
   const proto: Object = Object.getPrototypeOf(new ctr)
   if (Object.getPrototypeOf(o) !== proto) {
      for (const k of fields(o)) {
         assert(delete o[k as keyof Object])
      }
      Object.setPrototypeOf(o, proto)
   }
}

// The (possibly already extant) versioned object uniquely identified by a memo-key.
export function at<K extends PersistentObject, T extends PersistentObject> (k: K, ctr: PersistentClass<T>, ...args: Persistent[]): T {
   assert(interned(k))
   let o: PersistentObject | undefined = __versionedObjs.get(k)
   if (o === undefined) {
      o = new ctr
      // This may massively suck, performance-wise. Could move to VersionedObject now we have ubiquitous constructors.
      Object.defineProperty(o, "__id", {
         value: k,
         enumerable: false
      })
      Object.defineProperty(o, "__history", {
         value: new Map,
         enumerable: false
      })
      __versionedObjs.set(k, o)
   } else {
      reclassify(o, ctr)
   }
   // Couldn't get datatype-generic construction to work because fields not created by "new ctr".
   o.constructor_(...args)
   return __commit(asVersioned(o)) as T
}

// Fresh keys represent inputs to the system.
export const ν: () => ExternalObject =
   (() => {
      let count: number = 0
      return () => {
         return ExternalObject.make(count++)
      }
   })()

// "State object" whose identity doesn't matter and whose contents we can access by key.
export interface ObjectState {
   [index: string]: Persistent
}

// Ensure previous value of state is equal to current value at an existing world.
// Used to implement LVar-style increasing semantics, but that was only needed for call-by-need.
function __assertEqualState (tgt: ObjectState, src: Object): void {
   const src_: ObjectState = src as ObjectState
   assert(tgt.constructor === src.constructor)
   assert(fields(tgt).length === fields(src).length)
   fields(tgt).forEach((k: string): void => {
      assert(tgt[k] === src_[k])
   })
}

function __copy (src: Object): ObjectState {
   const tgt: ObjectState = Object.create(src.constructor.prototype)
   __newState(tgt, src)
   return tgt
}

// Set contents of src to tgt; return whether anything changed.
function __newState (tgt: ObjectState, src: Object): boolean {
   let changed: boolean = __nonNull(tgt).constructor !== __nonNull(src.constructor)
   reclassify(tgt, classOf(src))
   const src_: ObjectState = src as ObjectState
   fields(src).forEach((k: string): void => {
      if (tgt[k] !== src_[k]) {
         tgt[k] = src_[k]
         changed = true
      }
   })
   return changed
}

// At a given world, enforce "increasing" (LVar) semantics. Only permit non-increasing changes at new worlds.
function __commit (o: Versioned<PersistentObject>): Object {
   if (o.__history.size === 0) {
      const state: ObjectState = __copy(o)
      o.__history.set(__w, state)
   } else {
      const [lastModified, state]: [World, ObjectState] = stateAt(o, __w)
      if (lastModified === __w) {
         __assertEqualState(state, o)
      } else {
         // Semantics of copy-on-write but inefficient - we create the copy even if we don't need it: 
         const prev: ObjectState = __copy(state)
         if (__newState(state, o)) {
            o.__history.set(lastModified, prev)
            o.__history.set(__w, state)
         }
      }
   }
   return o
}

// State of o at w, plus predecessor of w at which that state was set.
function stateAt (o: VersionedObject, w: World): [World, ObjectState] {
   const v: ObjectState | undefined = o.__history.get(w)
   if (v === undefined) {
      if (w.parent !== null) {
         return stateAt(o, w.parent)
      } else {
         return absurd("No initial state.")
      }
   } else {
      return [w, v]
   }
}

// Standardise what we mean by the fields of an object.
export function fields (o: Object): string[] {
   return Object.keys(o)
}

export function fieldVals (o: Object): Persistent[] {
   return fields(o).map(k => (o as ObjectState)[k])
}

export class World implements PersistentObject, Ord<World> {
   public parent: World | null

   constructor_ (
      parent: World | null
   ) {
      this.parent = parent
   }

   eq (w: World): boolean {
      return this === w
   }

   leq (w: World): boolean {
      return this === w || (this.parent !== null && this.parent.leq(w))
   }

   static make (parent: World | null) {
      return make(World, parent)
   }

   static newRevision (): World {
      return __w = World.make(__w)
   }

   static undo (): void {
      if (__w.parent !== null) {
         __w = __w.parent
      } else {
         absurd()
      }
   }
}

export let __w: World = World.make(null)
