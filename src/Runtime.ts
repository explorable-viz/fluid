import { __shallowCopy, absurd, assert, className, funName, make } from "./util/Core"
import { Eq } from "./util/Eq"

export interface Ctr<T> {
   new (): T
}

// An object which can be used as a key in an ES6 map (i.e. one for which equality is ===). In particular
// interned objects are persistent objects.
export class PersistentObject implements Eq<PersistentObject> {
   __PersistentObject (): void {
      // discriminator
   }

   eq (that: PersistentObject): boolean {
      return this === that
   }
}

// Defined only if tgt, src are upper-bounded (LVar-style merge). Symmetric.
function __shallowMergeAssign (tgt: Object, src: Object): void {
   assert(tgt.constructor === src.constructor)
   for (let x of Object.keys(src)) {
      const tgt_: any = tgt as any,
            src_: any = src as any
      if (tgt_[x] === null) {
         tgt_[x] = src_[x]
      } else
      if (src_[x] === null) {
         src_[x] = tgt_[x]
      } else {
         if (tgt_[x] instanceof VersionedObject || typeof tgt_[x] === "number" || typeof tgt_[x] === "string") {
            assert(tgt_[x].eq(src_[x]), `Address collision (different value for property "${x}").`, tgt, src)
         } else
         if (tgt_[x] instanceof Object) {
            __shallowMergeAssign(tgt_[x], src_[x])
         } else {
            absurd()
         }
      }
   }
}

export type Persistent = null | PersistentObject | string | number

// A memo key which is sourced externally to the system. (The name "External" exists in the global namespace.)
export class ExternalObject extends PersistentObject {
   id: number

   static make (id: number): ExternalObject {
      const this_: ExternalObject = make(ExternalObject, id)
      this_.id = id
      return this_
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

      // At a given version (there is only one, currently) enforce "increasing" (LVar) semantics.
   __version (): Object {
      if (this.__history.length === 0) {
         this.__history.push(__shallowCopy(this))
      } else {
         __shallowMergeAssign(this.__history[0], this)
      }
      return this
   }
}

// Keys must be "memo" objects (interned or persistent).
type InstancesMap = Map<PersistentObject, VersionedObject<PersistentObject>>
const __ctrInstances: Map<Ctr<VersionedObject>, InstancesMap> = new Map

// Allocate a blank object uniquely identified by a memo-key. Needs to be initialised afterwards.
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
