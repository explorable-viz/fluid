import { assert, className } from "./util/Core"
import { Ctr } from "./DataType"
import { Addr, PersistentObject } from "./Memo"

const __instances: Map<Addr, PersistentObject> = new Map

// Allocate a blank object uniquely identified by a memo-key. Needs to be initialised afterwards.
export function create <T extends PersistentObject> (α: Addr, ctr: Ctr<T>): T {
   var o: PersistentObject | undefined = __instances.get(α)
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

// Fresh keys represent inputs to the system.
export const ν: () => Addr =
   (() => {
      let count: number = 0
      return () => {
         return (count++).toString()
      }
   })()
