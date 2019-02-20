import { absurd, assert } from "./util/Core"
import { Persistent, PersistentObject, make } from "./util/Persistent"
import { Nil } from "./BaseTypes"
import { Env, EnvEntry, ExtendEnv } from "./Env"
import { Expr, Lex, ν } from "./Expr"
import { ValId } from "./Eval"
import { get, has } from "./FiniteMap"
import { instantiate } from "./Instantiate"
import { Value } from "./Traced"

import Trie = Expr.Trie
import VoidKont = Expr.VoidKont

export type PrimResult<K> = [Value, K]
type TrieCtr = (body: VoidKont) => Trie.Prim<VoidKont>
type Unary<T, V> = (x: T) => (α: PersistentObject) => V
type Binary<T, U, V> = (x: T, y: U) => (α: PersistentObject) => V

// Parser guarantees that values/patterns respect constructor signatures. 
// TODO: rename to avoid confusion with Match.match.
function match<K extends Expr.Kont<K>> (v: Value, σ: Trie<K>): PrimResult<K> {
   if (v instanceof Value.PrimOp && (Trie.Fun.is(σ) || Trie.Top.is(σ))) {
      return [v, σ.κ]
   } else 
   if (v instanceof Value.ConstInt && (Trie.ConstInt.is(σ) || Trie.Top.is(σ))) {
      return [v, σ.κ]
   } else 
   if (v instanceof Value.ConstStr && (Trie.ConstStr.is(σ) || Trie.Top.is(σ ))) {
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
      } else
      if (Trie.Top.is(σ)) {
         return [v, σ.κ]
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

   invoke<K extends Expr.Kont<K>> (v: Value, σ: Trie<K>): (k: ValId) => PrimResult<K> {
      return k => match(this.op(v)(k), σ)
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

   invoke<K extends Expr.Kont<K>> (v1: Value, v2: Value, σ: Trie<K>): (k: ValId) => PrimResult<K> {
      return k => match(this.op(v1, v2)(k), σ)
   }
} 

export abstract class PrimOp implements PersistentObject {
   name: string
   abstract constructor_ (...args: Persistent[]): void // TS requires duplicate def
}

export class UnaryOp extends PrimOp {
   σ: Trie.Prim<VoidKont>
   b: UnaryBody

   constructor_ (
      name: string, 
      σ: Trie.Prim<VoidKont>,
      b: UnaryBody
   ) {
      this.name = name
      this.σ = σ
      this.b = b
   }

   static make (name: string, σ: Trie.Prim<VoidKont>, b: UnaryBody): UnaryOp {
      return make(UnaryOp, name, σ, b)
   }

   static make_<T extends Value, V extends Value> (op: Unary<T, V>, trie: TrieCtr): UnaryOp {
      return UnaryOp.make(op.name, trie(VoidKont.make()), UnaryBody.make(op))
   }
}

export class BinaryOp extends PrimOp {
   σ1: Trie.Prim<VoidKont>
   σ2: Trie.Prim<VoidKont> 
   b: BinaryBody

   constructor_ (
      name: string, 
      σ1: Trie.Prim<VoidKont>, 
      σ2: Trie.Prim<VoidKont>, 
      b: BinaryBody
   ) {
      this.name = name
      this.σ1 = σ1
      this.σ2 = σ2
      this.b = b
   }

   static make (name: string, σ1: Trie.Prim<VoidKont>, σ2: Trie.Prim<VoidKont>, b: BinaryBody): BinaryOp {
      return make(BinaryOp, name, σ1, σ2, b)
   }

   static make_<T extends Value, U extends Value, V extends Value> (op: Binary<T, U, V>, trie1: TrieCtr, trie2: TrieCtr): BinaryOp {
      return BinaryOp.make(op.name, trie1(VoidKont.make()), trie2(VoidKont.make()), BinaryBody.make(op))
   }
}

const unaryOps: Map<string, UnaryOp> = new Map([
   [error.name, UnaryOp.make_(error, Trie.ConstStr.make)],
   [intToString.name, UnaryOp.make_(intToString, Trie.ConstInt.make)],
])
   
export const binaryOps: Map<string, BinaryOp> = new Map([
   ["-", BinaryOp.make_(minus, Trie.ConstInt.make, Trie.ConstInt.make)],
   ["+", BinaryOp.make_(plus, Trie.ConstInt.make, Trie.ConstInt.make)],
   ["*", BinaryOp.make_(times, Trie.ConstInt.make, Trie.ConstInt.make)],
   ["/", BinaryOp.make_(div, Trie.ConstInt.make, Trie.ConstInt.make)],
   ["==", BinaryOp.make_(equalInt, Trie.ConstInt.make, Trie.ConstInt.make)],
   ["===", BinaryOp.make_(equalStr, Trie.ConstStr.make, Trie.ConstStr.make)],
   [">", BinaryOp.make_(greaterInt, Trie.ConstInt.make, Trie.ConstInt.make)],
   [">>", BinaryOp.make_(greaterStr, Trie.ConstStr.make, Trie.ConstStr.make)],
   ["<", BinaryOp.make_(lessInt, Trie.ConstInt.make, Trie.ConstInt.make)],
   ["<<", BinaryOp.make_(lessStr, Trie.ConstStr.make, Trie.ConstStr.make)],
   ["++", BinaryOp.make_(concat, Trie.ConstStr.make, Trie.ConstStr.make)]
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
   const ρ_0: Env = Env.empty()
   let ρ: Env = Env.empty()
   unaryOps.forEach((op: UnaryOp, x: string): void => {
      ρ = ExtendEnv.make(ρ, x, EnvEntry.make(ρ_0, Nil.make(), instantiate(ρ_0, Expr.PrimOp.at(ν(), op))))
   })
   return ρ
}
