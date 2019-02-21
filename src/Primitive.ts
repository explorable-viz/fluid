import { absurd, assert } from "./util/Core"
import { Persistent, PersistentObject, ν, make } from "./util/Persistent"
import { Nil } from "./BaseTypes"
import { Env, ExtendEnv } from "./Env"
import { Expr, Lex } from "./Expr"
import { Tagged, TraceId, ValId } from "./Eval"
import { get, has } from "./FiniteMap"
import { Traced, Value } from "./Traced"

import Empty = Traced.Empty
import Trie = Expr.Trie

export type PrimResult<K> = [Value, K]
type Unary<T, V> = (x: T) => (α: PersistentObject) => V
type Binary<T, U, V> = (x: T, y: U) => (α: PersistentObject) => V

// Parser guarantees that values/patterns respect constructor signatures. 
// TODO: rename to avoid confusion with Match.match.
export function match<K extends Expr.Kont<K>> (v: Value, σ: Trie<K>): PrimResult<K> {
   if (v instanceof Value.PrimOp && Trie.Fun.is(σ)) {
      return [v, σ.κ]
   } else
   if (v instanceof Value.Constr) {
      assert(v.args.length === 0, "Primitives must return nullary values.")
      if (Trie.Constr.is(σ) && has(σ.cases, v.ctr.str)) {
         const Π: Expr.Args<K> = get(σ.cases, v.ctr.str)!
         if (Expr.Args.End.is(Π)) {
            return [v, Π.κ]
         } else {
            return absurd()
         }
      } else {
         return absurd()
      }
   } else {
      return assert(false, "Primitive demand mismatch.", v, σ)
   }
}

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

function __true (k: ValId): Value.Constr {
   return Value.Constr.at(k, Lex.Ctr.make("True"), Nil.make())
}

function __false (k: ValId): Value.Constr {
   return Value.Constr.at(k, Lex.Ctr.make("False"), Nil.make())
}

// Used to take arbitrary value as additional argument, but now primitives have primitive arguments.
export function error (message: Value.ConstStr): (α: PersistentObject) => Value {
   return assert(false, "LambdaCalc error:\n" + message.val)
}

export function intToString (x: Value.ConstInt): (k: ValId) => Value.ConstStr {
   return k => Value.ConstStr.at(k, x.toString())
}

// No longer support overloaded functions, since the demand-indexed semantics is non-trivial.
export function equalInt (x: Value.ConstInt, y: Value.ConstInt): (k: ValId) => Value.Constr {
   return k => x.val === y.val ? __true(k) : __false(k)
}

export function equalStr (x: Value.ConstStr, y: Value.ConstStr): (k: ValId) => Value.Constr {
   return k => x.val === y.val ? __true(k) : __false(k)
}

export function greaterInt (x: Value.ConstInt, y: Value.ConstInt): (k: ValId) => Value.Constr {
   return k => x.val > y.val ? __true(k) : __false(k)
}

export function greaterStr (x: Value.ConstStr, y: Value.ConstStr): (k: ValId) => Value.Constr {
   return k => x.val > y.val ? __true(k) : __false(k)
}

export function lessInt (x: Value.ConstInt, y: Value.ConstInt): (k: ValId) => Value.Constr {
   return k => x.val > y.val ? __true(k) : __false(k)
}

export function lessStr (x: Value.ConstStr, y: Value.ConstStr): (k: ValId) => Value.Constr {
   return k => x.val > y.val ? __true(k) : __false(k)
}

export function minus (x: Value.ConstInt, y: Value.ConstInt): (k: ValId) => Value.ConstInt {
   return k => Value.ConstInt.at(k, x.val - y.val)
}

export function plus (x: Value.ConstInt, y: Value.ConstInt): (k: ValId) => Value.ConstInt {
   return k => Value.ConstInt.at(k, x.val + y.val)
}

export function times (x: Value.ConstInt, y: Value.ConstInt): (k: ValId) => Value.ConstInt {
   return k => Value.ConstInt.at(k, x.val * y.val)
}

export function div (x: Value.ConstInt, y: Value.ConstInt): (k: ValId) => Value.ConstInt {
   // Apparently this will round in the right direction.
   return k => Value.ConstInt.at(k, ~~(x.val / y.val))
}

export function concat (x: Value.ConstStr, y: Value.ConstStr): (k: ValId) => Value.ConstStr {
   return k => Value.ConstStr.at(k, x.val + y.val)
}

// Only primitive with identifiers as names are first-class, and therefore appear in the prelude.
export function prelude (): Env {
   let ρ: Env = Env.empty()
   unaryOps.forEach((op: UnaryOp, x: string): void => {
      const e: Expr = Expr.PrimOp.at(ν(), op),
            k: TraceId = Tagged.make(e, "trace"),
            kᵥ: ValId = Tagged.make(e, "val")
      ρ = ExtendEnv.make(ρ, x, Traced.make(Empty.at(k), Value.PrimOp.at(kᵥ, op)))
   })
   return ρ
}
