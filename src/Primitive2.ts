import { as, assert } from "./util/Core"
import { Bool, Pair, true_, false_ } from "./BaseTypes2"
import { Env, Func, emptyEnv, extendEnv } from "./Func2"
import { Num, Str, _, Value, make, num, str } from "./Value2"

type Unary<T, V> = (x: T) => V
type Binary<T, U, V> = (x: T, y: U) => V

// In the following two classes, we store the operation without generic type parameters, as fields can't
// have polymorphic type. Then access the operation via a method and reinstate the polymorphism via a cast.

export abstract class PrimOp extends Func {
   name: string = _
}

export class UnaryOp extends PrimOp {
   op: Unary<Value, Value> = _

   __apply (v: Value): Value {
      return this.op(v)
   }
}

function unary (name: string, op: Unary<Value, Value>): UnaryOp {
   return make(UnaryOp, name, op)
}

function unary_<T extends Value, V extends Value> (op: Unary<T, V>): UnaryOp {
   return unary(op.name, op)
}

export class BinaryOp extends PrimOp {
   op: Binary<Value, Value, Value> = _

   __apply ({ fst: v1, snd: v2 }: Pair<Value, Value>): Value {
      return this.op(v1, v2)
   }
}

function binary (name: string, op: Binary<Value, Value, Value>): BinaryOp {
   return make(BinaryOp, name, op)
}

function binary_<T extends Value, U extends Value, V extends Value> (op: Binary<T, U, V>): BinaryOp {
   return binary(op.name, op)
}

// Primitives with identifiers as names are unary and first-class.
const unaryOps: Map<string, UnaryOp> = new Map([
   [ceiling.name, unary_(ceiling)],
   [error.name, unary_(error)],
   [intToString.name, unary_(intToString)],
])
   
export const binaryOps: Map<string, BinaryOp> = new Map([
   ["-", binary_(minus)],
   ["+", binary_(plus)],
   ["*", binary_(times)],
   ["/", binary_(div)],
   ["==", binary_(equalInt)],
   ["===", binary_(equalStr)],
   [">", binary_(greaterInt)],
   [">>", binary_(greaterStr)],
   ["<", binary_(lessInt)],
   ["<<", binary_(lessStr)],
   ["++", binary_(concat)]
])

export function ceiling (x: Num): Num {
   return num(Math.ceil(x.val))
}

// Used to take arbitrary value as additional argument, but now primitives have primitive arguments.
export function error (message: Str): Value {
   return assert(false, "LambdaCalc error:\n" + message.val)
}

export function intToString (x: Num): Str {
   return str(x.val.toString())
}

// No longer support overloaded functions, since the pattern-matching semantics is non-trivial; might require typecase.
export function equalInt (x: Num, y: Num): Bool {
   return as(x, Num).val === as(y, Num).val ? true_() : false_()
}

export function equalStr (x: Str, y: Str): Bool {
   return as(x, Str).val === as(y, Str).val ? true_() : false_()
}

export function greaterInt (x: Num, y: Num): Bool {
   return as(x, Num).val > as(y, Num).val ? true_() : false_()
}

export function greaterStr (x: Str, y: Str): Bool {
   return as(x, Str).val > as(y, Str).val ? true_() : false_()
}

export function lessInt (x: Num, y: Num): Bool {
   return as(x, Num).val < as(y, Num).val ? true_() : false_()
}

export function lessStr (x: Str, y: Str): Bool {
   return as(x, Str).val < as(y, Str).val ? true_() : false_()
}

export function minus (x: Num, y: Num): Num {
   return num(as(x, Num).val - as(y, Num).val)
}

export function plus (x: Num, y: Num): Num {
   return num(as(x, Num).val + as(y, Num).val)
}

export function times (x: Num, y: Num): Num {
   return num(as(x, Num).val * as(y, Num).val)
}

// If we want integer division, apparently ~~(x / y) will round in the right direction.
export function div (x: Num, y: Num): Num {
   return num(as(x, Num).val / as(y, Num).val)
}

export function concat (x: Str, y: Str): Str {
   return str(as(x, Str).val + as(y, Str).val)
}

// Only primitive with identifiers as names are first-class, and therefore appear in the prelude.
export function createPrelude (): Env {
   let ρ: Env = emptyEnv()
   unaryOps.forEach((op: UnaryOp, x: string): void => {
      ρ = extendEnv(ρ, x, op)
   })
   return ρ
}
