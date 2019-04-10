import { Annotation, ann } from "./util/Annotated"
import { as, assert } from "./util/Core"
import { Persistent, PersistentObject, make } from "./util/Persistent"
import { ν } from "./util/Versioned"
import { nil } from "./BaseTypes"
import { Env, ExtendEnv } from "./Env"
import { Value } from "./ExplVal"
import { Expr, Lex } from "./Expr"
import { ValId, tagged } from "./Eval"

import ConstNum = Value.ConstNum
import ConstStr = Value.ConstStr
import Constr = Value.Constr

export type PrimResult<K> = [Value, K]
type Unary<T, V> = (x: T) => (k: PersistentObject, α: Annotation) => V
type Binary<T, U, V> = (x: T, y: U) => (k: PersistentObject, α: Annotation) => V

// In the following two classes, we store the operation without generic type parameters, as fields can't
// have polymorphic type. Then access the operation via a method and reinstate the polymorphism via a cast.

export class UnaryBody implements PersistentObject {
   op: Unary<Value, Value>

   constructor_ (op: Unary<Value, Value>) {
      this.op = op
   }

   static make<T extends Value, V extends Value> (op: Unary<T, V>): UnaryBody {
      return make(UnaryBody, op)
   }
} 

export class BinaryBody implements PersistentObject {
   op: Binary<Value, Value, Value>

   constructor_ (op: Binary<Value, Value, Value>) {
      this.op = op
   }

   static make<T extends Value, U extends Value, V extends Value> (op: Binary<T, U, V>): BinaryBody {
      return make(BinaryBody, op)
   }
} 

export abstract class PrimOp implements PersistentObject {
   name: string
   abstract constructor_ (...v̅: Persistent[]): void
}

export class UnaryOp extends PrimOp {
   b: UnaryBody

   constructor_ (name: string, b: UnaryBody) {
      this.name = name
      this.b = b
   }

   static make (name: string, b: UnaryBody): UnaryOp {
      return make(UnaryOp, name, b)
   }

   static make_<T extends Value, V extends Value> (op: Unary<T, V>): UnaryOp {
      return UnaryOp.make(op.name, UnaryBody.make(op))
   }
}

export class BinaryOp extends PrimOp {
   b: BinaryBody

   constructor_ (name: string, b: BinaryBody) {
      this.name = name
      this.b = b
   }

   static make (name: string, b: BinaryBody): BinaryOp {
      return make(BinaryOp, name, b)
   }

   static make_<T extends Value, U extends Value, V extends Value> (op: Binary<T, U, V>): BinaryOp {
      return BinaryOp.make(op.name, BinaryBody.make(op))
   }
}

const unaryOps: Map<string, UnaryOp> = new Map([
   [error.name, UnaryOp.make_(error)],
   [intToString.name, UnaryOp.make_(intToString)],
])
   
export const binaryOps: Map<string, BinaryOp> = new Map([
   ["-", BinaryOp.make_(minus)],
   ["+", BinaryOp.make_(plus)],
   ["*", BinaryOp.make_(times)],
   ["/", BinaryOp.make_(div)],
   ["==", BinaryOp.make_(equalInt)],
   ["===", BinaryOp.make_(equalStr)],
   [">", BinaryOp.make_(greaterInt)],
   [">>", BinaryOp.make_(greaterStr)],
   ["<", BinaryOp.make_(lessInt)],
   ["<<", BinaryOp.make_(lessStr)],
   ["++", BinaryOp.make_(concat)]
])

function __true (k: ValId, α: Annotation): Constr {
   return Value.constr(k, α, Lex.ctr("True"), nil())
}

function __false (k: ValId, α: Annotation): Constr {
   return Value.constr(k, α, Lex.ctr("False"), nil())
}

// Used to take arbitrary value as additional argument, but now primitives have primitive arguments.
export function error (message: ConstStr): (k: PersistentObject) => Value {
   return assert(false, "LambdaCalc error:\n" + message.val)
}

export function intToString (x: ConstNum): (k: ValId, α: Annotation) => ConstStr {
   return (k, α) => Value.constStr(k, α, x.toString())
}

// No longer support overloaded functions, since the pattern-matching semantics is non-trivial; might require typecase.
export function equalInt (x: ConstNum, y: ConstNum): (k: ValId, α: Annotation) => Constr {
   return as(x, ConstNum).val === as(y, ConstNum).val ? __true : __false
}

export function equalStr (x: ConstStr, y: ConstStr): (k: ValId, α: Annotation) => Constr {
   return as(x, ConstStr).val === as(y, ConstStr).val ? __true : __false
}

export function greaterInt (x: ConstNum, y: ConstNum): (k: ValId, α: Annotation) => Constr {
   return as(x, ConstNum).val > as(y, ConstNum).val ? __true : __false
}

export function greaterStr (x: ConstStr, y: ConstStr): (k: ValId, α: Annotation) => Constr {
   return as(x, ConstStr).val > as(y, ConstStr).val ? __true : __false
}

export function lessInt (x: ConstNum, y: ConstNum): (k: ValId, α: Annotation) => Constr {
   return as(x, ConstNum).val < as(y, ConstNum).val ? __true : __false
}

export function lessStr (x: ConstStr, y: ConstStr): (k: ValId, α: Annotation) => Constr {
   return as(x, ConstStr).val < as(y, ConstStr).val ? __true : __false
}

export function minus (x: ConstNum, y: ConstNum): (k: ValId, α: Annotation) => ConstNum {
   return (k, α) => Value.constNum(k, α, as(x, ConstNum).val - as(y, ConstNum).val)
}

export function plus (x: ConstNum, y: ConstNum): (k: ValId, α: Annotation) => ConstNum {
   return (k, α) => Value.constNum(k, α, as(x, ConstNum).val + as(y, ConstNum).val)
}

export function times (x: ConstNum, y: ConstNum): (k: ValId, α: Annotation) => ConstNum {
   return (k, α) => Value.constNum(k, α, as(x, ConstNum).val * as(y, ConstNum).val)
}

// If we want integer division, apparently ~~(x / y) will round in the right direction.
export function div (x: ConstNum, y: ConstNum): (k: ValId, α: Annotation) => ConstNum {
   return (k, α) => Value.constNum(k, α, as(x, ConstNum).val / as(y, ConstNum).val)
}

export function concat (x: ConstStr, y: ConstStr): (k: ValId, α: Annotation) => ConstStr {
   return (k, α) => Value.constStr(k, α, as(x, ConstStr).val + as(y, ConstStr).val)
}

// Only primitive with identifiers as names are first-class, and therefore appear in the prelude.
export function prelude (): Env {
   let ρ: Env = Env.empty()
   unaryOps.forEach((op: UnaryOp, x: string): void => {
      const e: Expr = Expr.primOp(ν(), ann.top, op),
            kᵥ: ValId = tagged(e, "v")
      ρ = ExtendEnv.make(ρ, x, Value.primOp(kᵥ, e.α, op))
   })
   return ρ
}
