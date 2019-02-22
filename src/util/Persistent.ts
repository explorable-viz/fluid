import { Class, __nonNull, absurd, as, assert, className, classOf } from "./Core"
import { Ord } from "./Ord"

// An object which can be used as a key in an ES6 map (i.e. one for which equality is ===). In particular
// interned objects are persistent objects. Interface so can be extended by VersionedObject, which it is
// convenient to have as an interface.
export interface PersistentObject {
   // ES6 only allows constructor calls via "new".
   constructor_ (...args: Persistent[]): void
}

// Functions are persistent to support primitives.
export type Persistent = null | PersistentObject | string | number | Function

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

type PersistentClass<T extends PersistentObject = PersistentObject> = new () => T

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
      for (const k of Object.keys(o)) {
         assert(delete o[k as keyof Object])
      }
      Object.setPrototypeOf(o, proto)
   }
}

// The (possibly already extant) versioned object uniquely identified by a memo-key.
export function at<K extends PersistentObject, T extends PersistentObject> (α: K, ctr: PersistentClass<T>, ...args: Persistent[]): T {
   assert(interned(α))
   let o: PersistentObject | undefined = __versionedObjs.get(α)
   if (o === undefined) {
      o = new ctr
      // This may massively suck, performance-wise. Could move to VersionedObject now we have ubiquitous constructors.
      Object.defineProperty(o, "__id", {
         value: α,
         enumerable: false
      })
      Object.defineProperty(o, "__history", {
         value: new Map,
         enumerable: false
      })
      __versionedObjs.set(α, o)
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

function __copy (src: Object): ObjectState {
   const tgt: ObjectState = Object.create(src.constructor.prototype),
         src_: ObjectState = src as ObjectState
   Object.keys(tgt).forEach((k: string): void => {
      tgt[k] = src_[k]
   })
   return tgt
}

// "State object" whose identity doesn't matter and whose contents we can access by key.
export interface ObjectState {
   [index: string]: Persistent
}

// Combine information from src into tgt and vice versa, at an existing world.
// Precondition: the two are upper-bounded; postcondition: they are equal.
function __mergeState (tgt: ObjectState, src: Object): void {
   const src_: ObjectState = src as ObjectState
   // TODO: remove hardcoded dependency on "Bot".
   if (className(tgt) === "Bot") {
      reclassify(tgt, classOf(src))
      Object.keys(src).forEach((k: string): void => {
         tgt[k] = src_[k]
      })
   } else 
   if (className(src) === "Bot") {
      reclassify(src, classOf(tgt))
      Object.keys(tgt).forEach((k: string): void => {
         src_[k] = tgt[k]
      })
   } else {
      assert(tgt.constructor === src.constructor)
      Object.keys(tgt).forEach((k: string): void => {
         tgt[k] = src_[k] = __merge(tgt[k], src_[k])
      })
   }
}

// Least upper bound of two upper-bounded objects.
function __merge (tgt: Persistent, src: Persistent): Persistent {
   if (src === null) {
      return tgt
   } else 
   if (tgt === null) {
      return src
   } else
   if (src === tgt) {
      return src
   } else
   if (tgt === null || src === null) {
      return absurd("Address collision (different child).")
   } else
   if (versioned(tgt) && versioned(src)) {
      return absurd("Address collision (different child).")
   } else
   if (interned(tgt) && interned(src)) {
      assert(
         tgt.constructor === src.constructor, 
         `Address collision (tgt ${className(tgt)} !== src ${className(src)}).`
      )
      const tgt_: ObjectState = tgt as Object as ObjectState, // retarded
            src_: ObjectState = src as Object as ObjectState,
            args: Persistent[] = Object.keys(tgt).map((k: string): Persistent => {
         return __merge(tgt_[k], src_[k])
      })
      return make(src.constructor as PersistentClass, ...args)
   } else {
      return absurd()
   }
}

// Assign contents of src to tgt; return whether anything changed. TODO: whether anything changed is not
// necessarily significant because of call-by-need: a slot may evolve from null to non-null during a run.
function __assignState (tgt: ObjectState, src: Object): boolean {
   let changed: boolean = __nonNull(tgt).constructor !== __nonNull(src.constructor)
   reclassify(tgt, classOf(src))
   const src_: ObjectState = src as ObjectState
   Object.keys(tgt).forEach((k: string): void => {
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
         __mergeState(state, o)
      } else {
         // Semantics of copy-on-write but inefficient - we create the copy even if we don't need it: 
         const prev: ObjectState = __copy(state)
         if (__assignState(state, o)) {
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

// Versioned objects can have different metatypes at different worlds; here we assume T is its type at the 
// current world.
export function getProp<T extends PersistentObject> (α: PersistentObject, cls: PersistentClass<T>, k: keyof T): Persistent {
   const o: PersistentObject = __nonNull(__versionedObjs.get(α)),
         oʹ: Versioned<T> = asVersioned(as<PersistentObject, T>(o, cls))
   return stateAt(oʹ, __w)[1][k as keyof ObjectState]
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
