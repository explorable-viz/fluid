import { __shallowCopy, __shallowEq, assert, className } from "./util/Core"
import { Ctr } from "./DataType"

// At a given version (there is only one, currently) enforce "single assignment" semantics.
Object.prototype.__version = function (): Object {
   if (this.__history.length === 0) {
      this.__history.push(__shallowCopy(this))
   } else {
      assert(__shallowEq(this, this.__history[0]))
   }
   return this
}

Object.defineProperty(Object.prototype, "__version", {
   enumerable: false
})

const __instances: Map<Addr, Object> = new Map

// Allocate a blank object uniquely identified by a memo-key. Needs to be initialised afterwards.
export function create <T> (α: Addr, ctr: Ctr<T>): T {
   var o: Object | undefined = __instances.get(α)
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
