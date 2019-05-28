import { as, assert } from "./util/Core"
import { Bool, trueʹ, falseʹ } from "./BaseTypes"
import { Id, Num, PrimOpTag, PrimValue, Str, _, Value, make } from "./Value"
import { Versioned, numʹ, strʹ } from "./Versioned"

type Unary<T, V> = (x: T) => (k: Id) => Versioned<V>
type Binary<T, U, V> = (x: T, y: U) => (k: Id) => Versioned<V>

// In the following two classes, we store the operation without generic type parameters, as fields can't
// have polymorphic type. Then access the operation via a method and reinstate the polymorphism via a cast.

export class PrimOp<Tag extends PrimOpTag> extends Value<Tag> {
   name: string = _
}

// First-class ops are not statically versioned; the values known statically to the interpreter are not
// versioned, but the copies manipulated at runtime are.
export class UnaryOp extends PrimOp<"UnaryOp"> {
   op: Unary<PrimValue, Value> = _
}

function unary (name: string, op: Unary<PrimValue, Value>): UnaryOp {
   return make(UnaryOp, name, op)
}

export class BinaryOp extends PrimOp<"BinaryOp"> {
   op: Binary<PrimValue, PrimValue, Value> = _
}

function binary (name: string, op: Binary<PrimValue, PrimValue, Value>): BinaryOp {
   return make(BinaryOp, name, op)
}

const ceiling = (x: Num) => (k: Id): Versioned<Num> => numʹ(k, Math.ceil(x.val))
// Used to take arbitrary value as additional argument, but now primitives have primitive arguments.
const error = (message: Str) => (k: Id): Versioned<Value> => assert(false, "LambdaCalc error:\n" + message.val)
const intToString = (x: Num) => (k: Id): Versioned<Str> => strʹ(k, x.val.toString())
// No longer support overloaded functions, since the pattern-matching semantics is non-trivial; might require typecase.
const equalInt = (x: Num, y: Num): (k: Id) => Versioned<Bool> => as(x, Num).val === as(y, Num).val ? trueʹ : falseʹ
const equalStr = (x: Str, y: Str): (k: Id) => Versioned<Bool> => as(x, Str).val === as(y, Str).val ? trueʹ : falseʹ
const greaterInt = (x: Num, y: Num): (k: Id) => Versioned<Bool> => as(x, Num).val > as(y, Num).val ? trueʹ : falseʹ
const greaterStr = (x: Str, y: Str): (k: Id) => Versioned<Bool> => as(x, Str).val > as(y, Str).val ? trueʹ : falseʹ
const lessInt = (x: Num, y: Num): (k: Id) => Versioned<Bool> => as(x, Num).val < as(y, Num).val ? trueʹ : falseʹ
const lessStr = (x: Str, y: Str): (k: Id) => Versioned<Bool> => as(x, Str).val < as(y, Str).val ? trueʹ : falseʹ
const minus = (x: Num, y: Num) => (k: Id): Versioned<Num> => numʹ(k, as(x, Num).val - as(y, Num).val)
const plus = (x: Num, y: Num) => (k: Id): Versioned<Num> => numʹ(k, as(x, Num).val + as(y, Num).val)
const times = (x: Num, y: Num) => (k: Id): Versioned<Num> => numʹ(k, as(x, Num).val * as(y, Num).val)
// If we want integer division, apparently ~~(x / y) will round in the right direction.
const div = (x: Num, y: Num) => (k: Id): Versioned<Num> => numʹ(k, as(x, Num).val / as(y, Num).val)
const concat = (x: Str, y: Str) => (k: Id): Versioned<Str> => strʹ(k, as(x, Str).val + as(y, Str).val)

// Convenience methods for building the maps.
function unary_<T extends PrimValue, V extends Value> (op: Unary<T, V>): UnaryOp {
   return unary(op.name, op)
}

function binary_<T extends PrimValue, U extends PrimValue, V extends Value> (op: Binary<T, U, V>): BinaryOp {
   return binary(op.name, op)
}

// Primitives with identifiers as names are unary and first-class.
export const unaryOps: Map<string, UnaryOp> = new Map([
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
