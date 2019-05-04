import { as } from "./util/Core"
import { Bool, true_, false_ } from "./BaseTypes2"
import { Num, Str, Value, make, num, str } from "./ExplVal2"

type Binary<T, U, V> = (x: T, y: U) => V

export class BinaryBody implements Value {
   op: Binary<Value, Value, Value>
} 

function binaryBody<T extends Value, U extends Value, V extends Value> (op: Binary<T, U, V>): BinaryBody {
   return make(BinaryBody, { op })
}

export abstract class PrimOp implements Value {
   name: string
}

export class BinaryOp extends PrimOp {
   b: BinaryBody
}

function binary (name: string, b: BinaryBody): BinaryOp {
   return make(BinaryOp, { name, b })
}

function binary_<T extends Value, U extends Value, V extends Value> (op: Binary<T, U, V>): BinaryOp {
   return binary(op.name, binaryBody(op))
}

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
