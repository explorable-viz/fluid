import { as, assert, className } from "./util/Core"
import { Ctr } from "./DataType"
import { UnaryOp, BinaryOp } from "./Primitive"
import * as P from "./Primitive"
import { Trace, Value, str } from "./Syntax"

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

// Populated by initPrimitives().
export var unaryOps: Map<string, UnaryOp> = new Map
export var binaryOps: Map<string, BinaryOp> = new Map

// Map primitives to their underlying JS operation.
function initPrimitives (): void {
   unaryOps.set("intToString", P.intToString)

   binaryOps.set(str.concat, P.concat)
   binaryOps.set(str.div, P.div)
   binaryOps.set(str.equal, P.equalOp)
   binaryOps.set(str.greaterT, P.greaterT)
   binaryOps.set(str.lessT, P.lessT)
   binaryOps.set(str.minus, P.minus)
   binaryOps.set(str.plus, P.plus)
   binaryOps.set(str.times, P.times)
   binaryOps.set("error", P.error)
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
