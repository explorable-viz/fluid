import { as, assert, className } from "./util/Core"
import { Ctr } from "./DataType"
import { Trace, Value } from "./Syntax"

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

// Previously used Object.assign, but that goes via getters/setters.
function __shallowCopy (src: Object): Object {
   const tgt: Object = new (src.constructor as { new(): Object } ) // lacks a construct signature
   for (let x of Object.keys(src)) {
      (<any>tgt)[x] = (<any>src)[x]
   }
   return tgt
}

function __shallowEq (o1: Object, o2: Object): boolean {
   assert(o1.constructor === o2.constructor)
   for (let x of Object.keys(o1)) {
      if ((<any>o1)[x] !== (<any>o2)[x]) {
         return false
      }
   }
   return true
}

export class Traced<T extends Value = Value> {
   trace: Trace
   val: T

   static at <T extends Value> (α: Addr, trace: Trace, val: T): Traced<T> {
      const this_: Traced<T> = create<Traced<T>>(α, Traced)
      this_.trace = as(trace, Trace)
      this_.val = val
      this_.__version()
      return this_
   }
}

export function as_ <T extends Value> (v: Traced<T>, ctr: Ctr<T>): Traced<T> {
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
