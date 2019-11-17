import { __check, as, assert, userError } from "./util/Core"
import { Bool, true_, false_ } from "./BaseTypes"
import { ExplValue, explValue } from "./DataValue"
import { Expl } from "./Expl"
import { Id, Num, PrimOpTag, PrimValue, Str, _, Value } from "./Value"
import { ν, at, num, str } from "./Versioned"

export type Unary<T, V> = (x: T) => (k: Id) => V
export type Binary<T, U, V> = (x: T, y: U) => (k: Id) => V

// In the following two classes, we store the operation without generic type parameters, as fields can't
// have polymorphic type. Then access the operation via a method and reinstate the polymorphism via a cast.

export class PrimOp<Tag extends PrimOpTag> extends Value<Tag> {
   name: string = _
}

export class UnaryOp extends PrimOp<"UnaryOp"> {
   op: Unary<PrimValue, Value> = _
}

export class BinaryOp extends PrimOp<"BinaryOp"> {
   op: Binary<PrimValue, PrimValue, Value> = _
}

const ceiling: Unary<Num, Num> = x => num(Math.ceil(x.val))

// Used to take arbitrary value as additional argument, but now primitives have primitive arguments.
const debugLog: Unary<Num | Str, Value> = v => (k: Id) => { 
   console.log(v); 
   return v 
}

const floor: Unary<Num, Num> = x => num(Math.floor(x.val))
const log: Unary<Num, Num> = x => num(Math.log(as(x, Num).val))
const numToStr: Unary<Num, Str> = x => str(x.val.toString())

// No longer support overloaded functions, since the pattern-matching semantics is non-trivial; might require typecase.
// If we want integer division, apparently ~~(x / y) will round in the right direction.
const div: Binary<Num, Num, Num> = (x, y) => (k: Id) => {
   const n: Num = num(as(x, Num).val / as(y, Num).val)(k)
   if (!isFinite(n.val)) {
      userError("Division by zero", x, y)
   }
   return n
}

const concat: Binary<Str, Str, Str> = (x, y) => str(as(x, Str).val + as(y, Str).val)

const equal: Binary<Num | Str, Num | Str, Bool> = (x, y) => {
   if (x instanceof Num && y instanceof Num) {
      return x.val === y.val ? true_() : false_()
   } else
   if (x instanceof Str && y instanceof Str) {
      return x.val === y.val ? true_() : false_()
   } else {
      return userError(`Expected ${Num.name} or ${Str.name}.`)
   }
}

const error: Unary<Str, Value> = message => assert(false, "LambdaCalc error:\n" + message.val)

const greaterEq: Binary<Num | Str, Num | Str, Bool> = (x, y) => {
   if (x instanceof Num && y instanceof Num) {
      return x.val >= y.val ? true_() : false_()
   } else
   if (x instanceof Str && y instanceof Str) {
      // string comparison delegates to central implementation for consistency
      return x.geq(y) ? true_() : false_()
   } else {
      return userError(`Expected ${Num.name} or ${Str.name}.`)
   }
}

const greater: Binary<Num, Num, Bool> = (x, y) => as(x, Num).val > as(y, Num).val ? true_() : false_()

const lessEq: Binary<Num, Num, Bool> = (x, y) => {
   if (x instanceof Num && y instanceof Num) {
      return as(x, Num).val <= as(y, Num).val ? true_() : false_()
   } else
   if (x instanceof Str && y instanceof Str) {
      return x.leq(y) ? true_() : false_()
   } else {
      return userError(`Expected ${Num.name} or ${Str.name}.`)
   }
}

const less: Binary<Num, Num, Bool> = (x, y) => as(x, Num).val < as(y, Num).val ? true_() : false_()
const minus: Binary<Num, Num, Num> = (x, y) => num(as(x, Num).val - as(y, Num).val)
const plus: Binary<Num, Num, Num> = (x, y) => num(as(x, Num).val + as(y, Num).val)
const pow: Binary<Num, Num, Num> = (x, y) => num(as(x, Num).val ** as(y, Num).val)
const times: Binary<Num, Num, Num> = (x, y) => num(as(x, Num).val * as(y, Num).val)

// Convenience methods for building the maps. Export to allow other modules to provide operations.
export function unary_<T extends PrimValue, V extends Value> (op: Unary<T, V>): ExplValue<UnaryOp> {
   return explValue(Expl.const_()(ν()), at(UnaryOp, op.name, op)(ν()))
}

export function binary_<T extends PrimValue, U extends PrimValue, V extends Value> (op: Binary<T, U, V>): ExplValue<BinaryOp> {
   return explValue(Expl.const_()(ν()), at(BinaryOp, op.name, op)(ν()))
}

// Primitives with identifiers as names are unary and first-class.
export const unaryOps: Map<string, ExplValue<UnaryOp>> = new Map([
   [ceiling.name, unary_(ceiling)],
   [debugLog.name, unary_(debugLog)],
   [error.name, unary_(error)],
   [floor.name, unary_(floor)],
   [log.name, unary_(log)],
   [numToStr.name, unary_(numToStr)]
])
   
export const binaryOps: Map<string, ExplValue<BinaryOp>> = new Map([
   ["-", binary_(minus)],
   ["+", binary_(plus)],
   ["*", binary_(times)],
   ["**", binary_(pow)],
   ["/", binary_(div)],
   ["==", binary_(equal)],
   [">", binary_(greater)],
   [">=", binary_(greaterEq)],
   ["<", binary_(less)],
   ["<=", binary_(lessEq)],
   ["++", binary_(concat)]
])
