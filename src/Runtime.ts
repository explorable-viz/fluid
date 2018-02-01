import { as, assert, className } from "./util/Core"
import { Ctr } from "./DataType"
import { Trace } from "./Syntax"

export class Traced<T = Object> {
   trace: Trace
   val: T
}

export function as_ <T> (v: Traced<T>, ctr: Ctr<T>): Traced<T> {
   if (v !== undefined) { // idiom for reifying datatypes means fields can be uninitialised
      as(v, Traced)
      as(v.val, ctr)
   }
   return v
}

const __instances: Map<Addr, Object> = new Map()

// Allocate a blank object uniquely identified by a memo-key. Needs to be initialised afterwards.
export function create <T> (α: Addr, ctr: Ctr<T>): T {
   var o: Object | undefined = __instances.get(α)
   if (o === undefined) {
      o = new ctr
      // This may massively suck, performance-wise.
      Object.defineProperty(o, '__addr', {
         value: α,
         enumerable: false
      })
      Object.defineProperty(o, '__history', {
         value: [],
         enumerable: false
      })
      __instances.set(α, o)
   } else {
      assert(o.constructor === ctr, 'Address collision.', α, className(o.constructor), className(ctr))
   }
   return <T>o
}

// Fresh keys represent inputs to the system.
export const ν: () => Addr =
   (() => {
      var count: number = 0
      return () => {
         return (count++).toString()
      }
   })()
