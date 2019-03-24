import { assert } from "./util/Core"
import { Persistent, PersistentObject, make } from "./util/Persistent"
import { ν } from "./util/Versioned"
import { ann } from "./Annotated"
import { nil } from "./BaseTypes"
import { Env, ExtendEnv } from "./Env"
import { Value } from "./ExplVal"
import { Expr, Lex } from "./Expr"
import { ValId, tagged } from "./Eval"

import { Annotation } from "./Annotated";

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
   abstract constructor_ (...args: Persistent[]): void // TS requires duplicate def
}

export class UnaryOp extends PrimOp {
   b: UnaryBody

   constructor_ (
      name: string, 
      b: UnaryBody
   ) {
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

   constructor_ (
      name: string, 
      b: BinaryBody
   ) {
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

function __true (k: ValId, α: Annotation): Value.Constr {
   return Value.constr(k, α, Lex.ctr("True"), nil())
}

function __false (k: ValId, α: Annotation): Value.Constr {
   return Value.constr(k, α, Lex.ctr("False"), nil())
}

// Used to take arbitrary value as additional argument, but now primitives have primitive arguments.
export function error (message: Value.ConstStr): (k: PersistentObject) => Value {
   return assert(false, "LambdaCalc error:\n" + message.val)
}

export function intToString (x: Value.ConstInt): (k: ValId, α: Annotation) => Value.ConstStr {
   return (k, α) => Value.constStr(k, α, x.toString())
}

// No longer support overloaded functions, since the demand-indexed semantics is non-trivial.
export function equalInt (x: Value.ConstInt, y: Value.ConstInt): (k: ValId, α: Annotation) => Value.Constr {
   return x.val === y.val ? __true : __false
}

export function equalStr (x: Value.ConstStr, y: Value.ConstStr): (k: ValId, α: Annotation) => Value.Constr {
   return x.val === y.val ? __true : __false
}

export function greaterInt (x: Value.ConstInt, y: Value.ConstInt): (k: ValId, α: Annotation) => Value.Constr {
   return x.val > y.val ? __true : __false
}

export function greaterStr (x: Value.ConstStr, y: Value.ConstStr): (k: ValId, α: Annotation) => Value.Constr {
   return x.val > y.val ? __true : __false
}

export function lessInt (x: Value.ConstInt, y: Value.ConstInt): (k: ValId, α: Annotation) => Value.Constr {
   return x.val < y.val ? __true : __false
}

export function lessStr (x: Value.ConstStr, y: Value.ConstStr): (k: ValId, α: Annotation) => Value.Constr {
   return x.val < y.val ? __true : __false
}

export function minus (x: Value.ConstInt, y: Value.ConstInt): (k: ValId, α: Annotation) => Value.ConstInt {
   return (k, α) => Value.constInt(k, α, x.val - y.val)
}

export function plus (x: Value.ConstInt, y: Value.ConstInt): (k: ValId, α: Annotation) => Value.ConstInt {
   return (k, α) => Value.constInt(k, α, x.val + y.val)
}

export function times (x: Value.ConstInt, y: Value.ConstInt): (k: ValId, α: Annotation) => Value.ConstInt {
   return (k, α) => Value.constInt(k, α, x.val * y.val)
}

export function div (x: Value.ConstInt, y: Value.ConstInt): (k: ValId, α: Annotation) => Value.ConstInt {
   // Apparently this will round in the right direction.
   return (k, α) => Value.constInt(k, α, ~~(x.val / y.val))
}

export function concat (x: Value.ConstStr, y: Value.ConstStr): (k: ValId, α: Annotation) => Value.ConstStr {
   return (k, α) => Value.constStr(k, α, x.val + y.val)
}

// Only primitive with identifiers as names are first-class, and therefore appear in the prelude.
export function prelude (): Env {
   let ρ: Env = Env.empty()
   unaryOps.forEach((op: UnaryOp, x: string): void => {
      const e: Expr = Expr.PrimOp.at(ν(), ann.top, op),
            kᵥ: ValId = tagged(e, "val")
      ρ = ExtendEnv.make(ρ, x, Value.primOp(kᵥ, e.α, op))
   })
   return ρ
}
