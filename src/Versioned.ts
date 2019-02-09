import { zip } from "./util/Array"
import { Class, NullaryClass, __nonNull, assert, absurd } from "./util/Core"
import { Ord } from "./util/Ord"
import { PersistentObject, ValueObject, make } from "./Persistent"

export abstract class InternedObject extends PersistentObject {
   eq (that: PersistentObject): boolean {
      return this === that
   }
}

// A memo key which is sourced externally to the system. (The name "External" exists in the global namespace.)
export class ExternalObject extends InternedObject {
   constructor (
      public id: number
   ) {
      super()
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

// Not sure what the T parameter is for here but Typescript seems to get confused without it.
function __blankCopy<T extends VersionedObject> (src: T): ObjectState {
   const tgt: ObjectState = Object.create(src.constructor.prototype)
   for (let x of Object.keys(src)) {
      tgt[x] = null
   }
   return tgt
}

// "State object" whose identity doesn't matter and whose contents we can access by key.
export interface ObjectState extends Object {
   [index: string]: Object | null
}

// Combine information from src into tgt and vice versa, at an existing world.
// Precondition: the two are upper-bounded; postcondition: they are equal.
function __mergeState (tgt: ObjectState, src: Object): void {
   assert(__nonNull(tgt).constructor === __nonNull(src.constructor))
   const src_: ObjectState = src as ObjectState
   Object.keys(tgt).forEach((k: string): void => {
      tgt[k] = src_[k] = __merge(tgt[k], src_[k])
   })
}

// Least upper bound of two upper-bounded objects.
function __merge (tgt: Object | null, src: Object | null): Object | null {
   if (src === null) {
      return tgt
   } else 
   if (tgt === null) {
      return src
   } else
   if (src === tgt) {
      return src
   } else 
   if (tgt instanceof VersionedObject && src instanceof VersionedObject) {
      return absurd("Address collision (different child).")
   } else
   if (tgt instanceof InternedObject && src instanceof InternedObject) {
      assert(tgt.constructor === src.constructor, "Address collision (different constructor).")
      const args: (Object | null)[] = Object.keys(tgt).map((k: string): Object | null => {
         return __merge(tgt[k as keyof Object], src[k as keyof Object])
      })
      return make(src.constructor as Class<InternedObject>, ...args)
   } else
   if (tgt instanceof ValueObject && src instanceof ValueObject) {
      assert(tgt.eq(src))
      return src
   } else {
      return absurd()
   }
}

// Assign contents of src to tgt; return whether anything changed. TODO: whether anything changed is not
// necessarily significant because of call-by-need: a slot may evolve from null to non-null during a run.
function __assignState (tgt: ObjectState, src: Object): boolean {
   assert(__nonNull(tgt).constructor === __nonNull(src.constructor))
   let changed: boolean = false
   const src_: ObjectState = src as ObjectState
   Object.keys(tgt).forEach((k: string): void => {
      if (!eq(tgt[k], src_[k])) {
         tgt[k] = src_[k]
         changed = true
      }
   })
   return changed
}

// At a given world, enforce "increasing" (LVar) semantics. Only permit non-increasing changes at new worlds.
function __commit (o: VersionedObject): Object {
   if (o.__history.size === 0) {
      const state: ObjectState = __blankCopy(o)
      __mergeState(state, o)
      o.__history.set(__w, state)
   } else {
      const [w, state]: [World, ObjectState] = stateAt(o, __w)
      if (w === __w) {
         __mergeState(state, o)
      } else {
         // Semantics of copy-on-write but inefficient - we create the copy even if we don't need it: 
         const prev: ObjectState = __blankCopy(o)
         __mergeState(prev, state)
         if (__assignState(state, o)) {
            o.__history.set(w, prev)
            o.__history.set(__w, state)
         }
      }
   }
   return o
}

function eq (tgt: Object | null, src: Object | null): boolean {
   if (tgt instanceof ValueObject && src instanceof ValueObject) {
      return tgt.eq(src)
   } else {
      return tgt === src
   }
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
export function getProp<T extends VersionedObject> (o: T, k: keyof T): Object | null {
   return stateAt(o, __w)[1][k as string]
}

export abstract class VersionedObject<K extends PersistentObject = PersistentObject> extends PersistentObject {
   // Initialise these at object creation (not enumerable).
   __history: Map<World, ObjectState> = undefined as any // history records only enumerable fields
   __id: K = undefined as any

   // ES6 only allows constructor calls via "new".
   abstract constructor_ (...args: (Object | null)[]): void

   eq (that: PersistentObject): boolean {
      return this === that
   }
}

// Keys must be "memo" (persistent) objects.
type InstancesMap = Map<PersistentObject, VersionedObject<PersistentObject>>
const __ctrInstances: Map<NullaryClass<VersionedObject>, InstancesMap> = new Map

// Datatype-generic construction.
export function constructor_ (this_: VersionedObject, ...args: (Object | null)[]): void {
   const ks: string[] = Object.keys(this_)
   assert(ks.length === args.length)
   zip(ks, args).forEach(([k, arg]: [string, Object | null]): void => {
      (this_ as Object as ObjectState)[k] = arg
   })
}

// The (possibly already extant) object uniquely identified by a memo-key.
export function at<K extends PersistentObject, T extends VersionedObject<K>> (α: K, ctr: NullaryClass<T>, ...args: (Object | null)[]): T {
   let instances: InstancesMap | undefined = __ctrInstances.get(ctr)
   if (instances === undefined) {
      instances = new Map
      __ctrInstances.set(ctr, instances)
   }
   let o: VersionedObject<K> | undefined = instances.get(α) as VersionedObject<K>
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
      instances.set(α, o)
   }
   // Couldn't get datatype-generic construction to work because fields not created by "new ctr".
   o.constructor_(...args)
   return __commit(o) as T
}

export class World extends InternedObject implements Ord<World> {
   constructor (public parent: World | null) {
      super()
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
}

export let __w: World = new World(null)
