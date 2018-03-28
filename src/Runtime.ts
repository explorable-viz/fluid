import { __shallowCopy, __shallowEq, assert, className } from "./util/Core"
import { Ctr } from "./DataType"
import { ops } from "./Primitive"
import * as P from "./Primitive"
import { Env, Expr, Value, str } from "./Syntax"

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

// Populated by initDataTypes(). Note that constructors are not (yet) first-class.
export const projections: Map<string, UnaryOp> = new Map

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

// Only primitives at the moment; eventually other library code. Fake "syntax" for primitives.
export function prelude (): Env {
   initPrimitives()

   const ρ: Env = new Map
   ops.forEach((op: Value.PrimOp) => {
      ρ.set(op.name, {ρ: new Map, δ: [], e: Expr.PrimOp.at(ν(), op)})
   })
   return ρ
}

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
      var count: number = 0
      return () => {
         return (count++).toString()
      }
   })()
