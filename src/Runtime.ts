import { Class, absurd, assert, className, funName, make, __nonNull } from "./util/Core"
import { Eq } from "./util/Eq"

export interface Ctr<T> {
   new (): T
}

// An object which can be used as a key in an ES6 map (i.e. one for which equality is ===). In particular
// interned objects are persistent objects.
export abstract class PersistentObject implements Eq<PersistentObject> {
   __PersistentObject (): void {
      // discriminator
   }

   // The implementations of these are all identical but this forces a concrete partitioning.
   abstract eq (that: PersistentObject): boolean
}

export abstract class InternedObject extends PersistentObject {
   eq (that: PersistentObject): boolean {
      return this === that
   }
}

function __blankCopy<T extends Object> (src: T): T {
   const tgt: T = Object.create(src.constructor.prototype)
   for (let x of Object.keys(src)) {
      (tgt as any)[x] = null
   }
   return tgt
}

// Argument tgtState is a "value object" whose identity doesn't matter but whose state represents what we currently 
// know about src. Precondition: the two are upper-bounded; postcondition is that they are equal.
export function __mergeAssign (tgtState: Object, src: VersionedObject) {
   assert(__nonNull(tgtState).constructor === __nonNull(src.constructor))
   const tgtState_: any = tgtState as any,
         src_: any = src as any
   Object.keys(tgtState).forEach((k: string): void => {
      tgtState_[k] = src_[k] = __merge(tgtState_[k], src_[k])
   })
}

// Least upper bound of two upper-bounded objects.
export function __merge (tgt: Object, src: Object): Object {
   if (src === null) {
      return tgt
   } else 
   if (tgt === null) {
      return src
   } else
   if (src === tgt) {
      return src
   } else {
      assert(tgt.constructor === src.constructor)
      assert(!(tgt instanceof VersionedObject), "Upper-bounded versioned objects have the same address")
      assert(tgt instanceof InternedObject) // ignore other case for now
      const args: any[] = Object.keys(tgt).map((k: string): any => {
         return __merge((tgt as any)[k], (src as any)[k])
      })
      // Two dubious assumptions, but hard to see another technique:
      // (1) entries are supplied in declaration-order (not guaranteed by language spec)
      // (2) constructor arguments also match declaration-order (easy constraint to violate)
      return make(src.constructor as Class<InternedObject>, args)
   }
}   

// Defined only if tgt, src are upper-bounded. Persistent objects are immutable, whereas versioned objects merge 
// (symmetrically) in an LVar-like way. Not convinced that there is a coherent design here.
export function __shallowMergeAssign (tgt: Object, src: Object): void {
   assert(tgt.constructor === src.constructor)
   for (let x of Object.keys(src)) {
      const tgt_: any = tgt as any,
            src_: any = src as any
      if (tgt_[x] === null) {
         if (tgt instanceof VersionedObject) {
            tgt_[x] = src_[x]
         }
      } else
      if (src_[x] === null) {
         if (src instanceof VersionedObject) {
            src_[x] = tgt_[x]
         }
      } else {
         if ((tgt_[x] instanceof VersionedObject || typeof tgt_[x] === "number" || typeof tgt_[x] === "string")) {
            assert(tgt_[x].eq(src_[x]), `Address collision (different value for property "${x}").`, tgt, src)
         } else
         // Interned child objects have distinct addresses iff they have different (but upper-bounded) 
         // content; only really practical (and indeed useful) to assert this in the distinct case.
         if (tgt_[x] instanceof PersistentObject && src_[x] instanceof PersistentObject) {
            if (!tgt_[x].eq(src_[x])) {
               __shallowMergeAssign(tgt_[x], src_[x])
            }
         } else
         // TODO: I think this case only applies to lexemes, but shouldn't they be VersionedObjects?
         if (tgt_[x] instanceof Object && src_[x] instanceof Object) {
            __shallowMergeAssign(tgt_[x], src_[x])
         } else {
            absurd()
         }
      }
   }
}

export type Persistent = null | PersistentObject | string | number

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

export class VersionedObject<K extends PersistentObject = PersistentObject> extends PersistentObject {
   // Initialise these at object creation (not enumerable).
   __history: Object[] = undefined as any // history records only enumerable fields
   __id: K = undefined as any

   eq (that: PersistentObject): boolean {
      return this === that
   }
      // At a given version (there is only one, currently) enforce "increasing" (LVar) semantics.
   __version (): Object {
      if (this.__history.length === 0) {
         this.__history.push(__blankCopy(this))
      }
      __mergeAssign(this.__history[0], this)
      return this
   }
}

// Keys must be "memo" objects (interned or persistent).
type InstancesMap = Map<PersistentObject, VersionedObject<PersistentObject>>
const __ctrInstances: Map<Ctr<VersionedObject>, InstancesMap> = new Map

// The (possibly already extant) object uniquely identified by a memo-key. Needs to be initialised afterwards.
// Unfortunately the Id type constraint is rather weak in TypeScript because of "bivariance".
export function create<K extends PersistentObject, T extends VersionedObject<K>> (α: K, ctr: Ctr<T>): T {
   let instances: InstancesMap | undefined = __ctrInstances.get(ctr)
   if (instances === undefined) {
      instances = new Map
      __ctrInstances.set(ctr, instances)
   }
   let o: VersionedObject<K> | undefined = instances.get(α) as VersionedObject<K>
   if (o === undefined) {
      o = Object.create(ctr.prototype) as T // new ctr doesn't work any more
      // This may massively suck, performance-wise. Define these here rather than on VersionedObject
      // to avoid constructors everywhere.
      Object.defineProperty(o, "__id", {
         value: α,
         enumerable: false
      })
      Object.defineProperty(o, "__history", {
         value: [],
         enumerable: false
      })
      instances.set(α, o)
   } else {
      // initialisation should always version, which will enforce single-assignment, so this additional
      // check strictly unnecessary. However failing now avoids weird ill-formed objects.
      assert(o.constructor === ctr, "Address collision (different constructor).", α, className(o), funName(ctr))
   }
   return o as T
}
