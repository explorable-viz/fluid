import { Annotated, Annotation, ann } from "./Annotated"
import { Class, __nonNull, absurd, assert, classOf } from "./Core"
import { Ord } from "./Ord"
import { MemoArgs, Persistent, PersistentClass, PersistentObject, make } from "./Persistent"
import { Some, Option, some, none } from "../BaseTypes" // TODO: fix upwards dependency

// Versioned objects are persistent objects that have state that varies across worlds.
export interface VersionedObject<K extends PersistentObject = PersistentObject> extends PersistentObject {
   // Initialise these at object creation (not enumerable).
   __history: Map<World, ObjectState> // history records only enumerable fields
   __id: K
}

export type Versioned<T> = T & VersionedObject

export function versioned (o: Persistent): o is VersionedObject {
   return (__nonNull(o) as any).__id !== undefined
}

export function asVersioned<T extends Persistent> (o: T): Versioned<T> {
   if (versioned(o)) {
      return o
   } else {
      return absurd()
   }
}

export function interned (o: Persistent): boolean {
   return !versioned(o)
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

// For versioned objects the map is not curried but takes an (interned) composite key. TODO: treating the constructor
// as part of the key isn't correct because objects can change class. To match the formalism, we need a notion of 
// "metatype" or kind, so that traces and values are distinguished, but within those "kinds" the class can change.
type VersionedObjects = Map<PersistentObject, PersistentObject>
const __versionedObjs: VersionedObjects = new Map

// The (possibly already extant) versioned object uniquely identified by a memo-key.
export function at<K extends PersistentObject, T extends PersistentObject> (k: K, ctr: PersistentClass<T>, ...args: MemoArgs): T {
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

// Fresh keys represent inputs to the system.
export const ν: () => ExternalObject =
   (() => {
      let count: number = 0
      return () => {
         return ExternalObject.make(count++)
      }
   })()

// "State object" whose identity doesn't matter and whose contents we can access by key. Might make sense to merge 
// this into PersistentObject but would probably require some shenanigans to exclude non-string properties.
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
      assert(tgt[k] === src_[k], "Incompatible field values", tgt, src_)
   })
}

function __copy (src: Object): ObjectState {
   const tgt: ObjectState = new (src.constructor as Class<ObjectState>)
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
      if (Some.is(w.parent)) {
         return stateAt(o, w.parent.t)
      } else {
         return absurd("No initial state.")
      }
   } else {
      return [w, v]
   }
}

// Standardise what we mean by the fields of an object. The fields of an object includes its annotation, so that
// enforcing single-assignment semantics includes annotations.
export function fields (o: Object): string[] {
   return Object.keys(o)
}

export function fieldVals (o: Object): MemoArgs {
   return fields(o).map(k => (o as ObjectState)[k])
}

export class World implements PersistentObject, Ord<World> {
   public parent: Option<World>

   constructor_ (parent: Option<World>) {
      this.parent = parent
   }

   eq (w: World): boolean {
      return this === w
   }

   leq (w: World): boolean {
      return this === w || (Some.is(this.parent) && this.parent.t.leq(w))
   }

   static make (parent: Option<World>) {
      return make(World, parent)
   }

   static revisions: number = 0

   static newRevision (): World {
      console.log(`At revision ${World.revisions++}`)
      return __w = World.make(some(__w))
   }
}

export let __w: World = World.make(none())

export abstract class AnnotatedVersioned extends Annotated {
   abstract constructor_ (...v̅: MemoArgs): void

   // Could avoid these shenanigans if we had AnnotatedValue as an explicit wrapper (depends on α being first argument).
   copyAt<T extends Annotated & PersistentObject> (k: PersistentObject, α: Annotation): T {
      const cls: PersistentClass<T> = classOf(this) as PersistentClass<Annotated & PersistentObject> as PersistentClass<T> // TS can't cope
      return at<PersistentObject, T>(k, cls, α, ...fieldVals(this).slice(1))
   }

   setα (α: Annotation): this {
      const hereʹ: Versioned<this> = asVersioned(this)
      hereʹ.copyAt(hereʹ.__id, α)
      return this
   }

   joinα (α: Annotation): this {
      const hereʹ: Versioned<this> = asVersioned(this)
      hereʹ.copyAt(hereʹ.__id, ann.join(this.α, α))
      return this
   }
}

// Memoising an imperative function makes any side effects idempotent. Not clear yet how to "partially" memoise LVar-like 
// functions like joinα, but setall isn't one of those.
export function setall<T extends PersistentObject> (tgt: T, α: Annotation): T {
   return setall_(tgt, α)
}

// An annotation lattice induces a lattice for any object that potentially contains annotations. They behave with imperative 
// LVar-like semantics, so although there is a notion of join/meet, we don't actually need to define them.
export function setall_<T extends PersistentObject> (tgt: T, α: Annotation): T {
   if (tgt instanceof AnnotatedVersioned) {
      tgt.setα(α)
   }
   fieldVals(tgt).forEach((v: Persistent): void => {
      if (v instanceof Object) { // annoying that PersistentObject isn't a class
         setall(v as PersistentObject, α) 
      }
   })
   return tgt
}
