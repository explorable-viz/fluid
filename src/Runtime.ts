import { __shallowCopy, __shallowEq, assert, className } from "./util/Core"

export interface Ctr<T> {
   new (): T
}

export type RawId = number

export class Id {
   __Id() {
      // descriminator
   }
}

export class PersistentObject<T extends Id> extends Object {
   // Initialise these properties at object creation.
   __history: this[] = undefined as any
   __id: T = undefined as any
   __version: () => Object = undefined as any
}

const __instances: Map<Id, PersistentObject<Id>> = new Map

// Allocate a blank object uniquely identified by a memo-key. Needs to be initialised afterwards.
// Unfortunately the Id type constraint is rather weak in TypeScript because of "bivariance".
export function create <I extends Id, T extends PersistentObject<I>> (α: I, ctr: Ctr<T>): T {
   let o: PersistentObject<I> | undefined = __instances.get(α) as PersistentObject<I>
   if (o === undefined) {
      o = Object.create(ctr.prototype) as T // new ctr doesn't work any more?
      // This may massively suck, performance-wise. Define these rather than on PersistentObject
      // to avoid constructors everywhere.
      Object.defineProperty(o, "__id", {
         value: α,
         enumerable: false
      })
      Object.defineProperty(o, "__history", {
         value: [],
         enumerable: false
      })
      // At a given version (there is only one, currently) enforce "single assignment" semantics.
      Object.defineProperty(o, "__version", {
         value: function (): Object {
            const this_: PersistentObject<I> = this as PersistentObject<I>
            if (this_.__history.length === 0) {
               this_.__history.push(__shallowCopy(this_))
            } else {
               assert(__shallowEq(this, this_.__history[0]))
            }
            return this
         },
         enumerable: false
      })
      __instances.set(α, o)
   } else {
      assert(o.constructor === ctr, "Address collision.", α, className(o.constructor), className(ctr))
   }
   return o as T
}
