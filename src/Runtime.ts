import { Class, assert, className, funName, make, __nonNull, absurd } from "./util/Core"
import { Ord } from "./util/Ord"
import { PersistentObject } from "./util/Core"

export interface Ctr<T> {
   new (): T
}

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

function __blankCopy<T extends Object> (src: T): T {
   const tgt: T = Object.create(src.constructor.prototype)
   for (let x of Object.keys(src)) {
      (tgt as any)[x] = null
   }
   return tgt
}

enum MergeResult {
   Unchanged,     // no increase in knowledge
   Increasing,    // strictly increasing; permitted at a world
   NonIncreasing  // neither of the above; only permitted at a new world
}

// Argument tgtState is a "value object" whose identity doesn't matter but whose state represents what we currently 
// know about src. Precondition: the two are upper-bounded; postcondition is that they are equal.
function __mergeAssign (tgtState: Object, src: VersionedObject): MergeResult {
   assert(__nonNull(tgtState).constructor === __nonNull(src.constructor))
   const tgtState_: any = tgtState as any,
         src_: any = src as any
   let result: MergeResult = MergeResult.Unchanged
   Object.keys(tgtState).forEach((k: string): void => {
      const [v, resultʹ]: [Object, MergeResult] = __merge(tgtState_[k], src_[k])
      result = Math.max(result, resultʹ)
      tgtState_[k] = src_[k] = v
   })
   return result
}

// Least upper bound of two upper-bounded objects.
function __merge (tgt: Object, src: Object): [Object, MergeResult] {
   if (src === null) {
      return [tgt, MergeResult.Unchanged]
   } else 
   if (tgt === null) {
      return [src, MergeResult.Increasing]
   } else
   if (src === tgt) {
      return [src, MergeResult.Unchanged]
   } else
   // upper-bounded versioned objects have the same address
   if (tgt instanceof VersionedObject || tgt.constructor !== src.constructor) {
      return [src, MergeResult.NonIncreasing]
   } else {
      assert(tgt instanceof InternedObject)
      let result: MergeResult = MergeResult.Unchanged
      const args: Object[] = Object.keys(tgt).map((k: string): Object => {
         let [arg, resultʹ] = __merge((tgt as any)[k], (src as any)[k])
         result = Math.max(result, resultʹ)
         return arg
      })
      // Two dubious assumptions, but hard to see another technique:
      // (1) entries are supplied in declaration-order (not guaranteed by language spec)
      // (2) constructor arguments also match declaration-order (easy constraint to violate)
      return [make(src.constructor as Class<InternedObject>, ...args), result]
   }
}   

function mostRecent (history: Map<World, Object>, w: World): [World, Object] {
   const v: Object | undefined = history.get(w)
   if (v === undefined) {
      if (w.parent !== null) {
         return mostRecent(history, w.parent)
      } else {
         return absurd("No initial state.")
      }
   } else {
      return [w, v]
   }
}

export abstract class VersionedObject<K extends PersistentObject = PersistentObject> extends PersistentObject {
   // Initialise these at object creation (not enumerable).
   __history: Map<World, Object> = undefined as any // history records only enumerable fields
   __id: K = undefined as any

   // ES6 only allows constructor calls via "new".
   abstract constructor_ (...args: any[]): void

   eq (that: PersistentObject): boolean {
      return this === that
   }
      // At a given world, enforce "increasing" (LVar) semantics. Only permit non-increasing changes at new worlds.
   __version (): Object {
      if (this.__history.size === 0) {
         const state: Object = __blankCopy(this)
         assert(__mergeAssign(state, this) <= MergeResult.Increasing)
         this.__history.set(__w, state)
      } else {
         const [w, state]: [World, Object] = mostRecent(this.__history, __w),
               result: MergeResult = __mergeAssign(state, this)
         if (w === __w) {
            assert(result !== MergeResult.NonIncreasing, "Address collision.")   
         } else
         if (result !== MergeResult.Unchanged) {
            this.__history.set(__w, state)
         }
      }
      return this
   }
}

// Keys must be "memo" objects (interned or persistent).
type InstancesMap = Map<PersistentObject, VersionedObject<PersistentObject>>
const __ctrInstances: Map<Ctr<VersionedObject>, InstancesMap> = new Map

// The (possibly already extant) object uniquely identified by a memo-key. Needs to be initialised afterwards.
export function at<K extends PersistentObject, T extends VersionedObject<K>> (α: K, ctr: Ctr<T>, ...args: any[]): T {
   let instances: InstancesMap | undefined = __ctrInstances.get(ctr)
   if (instances === undefined) {
      instances = new Map
      __ctrInstances.set(ctr, instances)
   }
   let o: VersionedObject<K> | undefined = instances.get(α) as VersionedObject<K>
   if (o === undefined) {
      o = Object.create(ctr.prototype) as T
      // This may massively suck, performance-wise. Define these here rather than on VersionedObject
      // to avoid constructors everywhere.
      Object.defineProperty(o, "__id", {
         value: α,
         enumerable: false
      })
      Object.defineProperty(o, "__history", {
         value: new Map,
         enumerable: false
      })
      instances.set(α, o)
   } else {
      // Initialisation calls __version, which enforces single-assignment, so this additional
      // check strictly unnecessary. However failing now avoids weird ill-formed objects.
      assert(o.constructor === ctr, "Address collision (different constructor).", α, className(o), funName(ctr))
   }
   o.constructor_(...args)
   return o.__version() as T
}

class World extends InternedObject implements Ord<World> {
   constructor (public parent: World | null) {
      super()
   }

   leq (w: World): boolean {
      return this === w || (this.parent !== null && this.parent.leq(w))
   }

   static make (parent: World | null) {
      return make(World, parent)
   }
}

const __w: World = new World(null)
