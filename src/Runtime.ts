import { assert, className } from "./util/Core"
import { Ctr } from "./DataType"
import { Id, PersistentObject } from "./Memo"

const __instances: Map<Id, PersistentObject<Id>> = new Map

// Allocate a blank object uniquely identified by a memo-key. Needs to be initialised afterwards.
// Unfortunately the Id type constraint is rather weak in TypeScript because of "bivariance".
export function create <I extends Id, T extends PersistentObject<I>> (α: I, ctr: Ctr<T>): T {
   var o: PersistentObject<I> | undefined = __instances.get(α) as PersistentObject<I>
   if (o === undefined) {
      o = new ctr
      // This may massively suck, performance-wise.
      Object.defineProperty(o, "__addr", {
         value: α,
         enumerable: false
      })
      Object.defineProperty(o, "__history", {
         value: [],
         enumerable: false
      })
      __instances.set(α, o)
   } else {
      assert(o.constructor === ctr, "Address collision.", α, className(o.constructor), className(ctr))
   }
   return o as T
}
