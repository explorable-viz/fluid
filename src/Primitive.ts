import { absurd, assert } from "./util/Core"
import { PersistentObject, make } from "./util/Persistent"
import { InternedObject, ν } from "./util/Versioned"
import { Nil } from "./BaseTypes"
import { Env, EnvEntry, ExtendEnv } from "./Env"
import { Expr, Lex } from "./Expr"
import { get, has } from "./FiniteMap"
import { instantiate } from "./Instantiate"
import { Traced, Value } from "./Traced"

import Args = Traced.Args
import Trie = Traced.Trie

export type PrimResult<K> = [Value, K]
type TrieCtr = (body: null) => Trie.Prim<null>
type Unary<T, V> = (x: T) => (α: PersistentObject) => V
type Binary<T, U, V> = (x: T, y: U) => (α: PersistentObject) => V

// Parser guarantees that values/patterns respect constructor signatures. 
// TODO: rename to avoid confusion with Match.match.
function match<K> (v: Value, σ: Trie<K>): PrimResult<K> {
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
         const Π: Args<K> = get(σ.cases, v.ctr.str)!
         if (Args.End.is(Π)) {
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

export class UnaryBody extends InternedObject {
   op: Unary<Value, Value>

   constructor (op: Unary<Value, Value>) {
      super()
      this.op = op
   }

   static make<T extends Value, V extends Value> (op: Unary<T, V>): UnaryBody {
      return make(UnaryBody, op)
   }

   invoke<K> (v: Value, σ: Trie<K>): (α: PersistentObject) => PrimResult<K> {
      return α => match(this.op(v)(α), σ)
   }
} 

export class BinaryBody extends InternedObject {
   op: Binary<Value, Value, Value>

   constructor (op: Binary<Value, Value, Value>) {
      super()
      this.op = op
   }

   static make<T extends Value, U extends Value, V extends Value> (op: Binary<T, U, V>): BinaryBody {
      return make(BinaryBody, op)
   }

   invoke<K> (v1: Value, v2: Value, σ: Trie<K>): (α: PersistentObject) => PrimResult<K> {
      return α => match(this.op(v1, v2)(α), σ)
   }
} 

export abstract class PrimOp extends InternedObject {
   constructor (public name: string) {
      super()
   }
}

export class UnaryOp extends PrimOp {
   constructor (
      name: string, 
      public σ: Trie.Prim<null>, 
      public b: UnaryBody) {
      super(name)
   }

   static make (name: string, σ: Trie.Prim<null>, b: UnaryBody): UnaryOp {
      return make(UnaryOp, name, σ, b)
   }

   static make_<T extends Value, V extends Value> (op: Unary<T, V>, trie: TrieCtr): UnaryOp {
      return UnaryOp.make(op.name, trie(null), UnaryBody.make(op))
   }
}

export class BinaryOp extends PrimOp {
   constructor (
      name: string, 
      public σ1: Trie.Prim<null>, 
      public σ2: Trie.Prim<null>, 
      public b: BinaryBody
   ) {
      super(name)
   }

   static make (name: string, σ1: Trie.Prim<null>, σ2: Trie.Prim<null>, b: BinaryBody): BinaryOp {
      return make(BinaryOp, name, σ1, σ2, b)
   }

   static make_<T extends Value, U extends Value, V extends Value> (op: Binary<T, U, V>, trie1: TrieCtr, trie2: TrieCtr): BinaryOp {
      return BinaryOp.make(op.name, trie1(null), trie2(null), BinaryBody.make(op))
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

function __true (α: PersistentObject): Value.Constr {
   return Value.Constr.at(α, new Lex.Ctr("True"), Nil.make())
}

function __false (α: PersistentObject): Value.Constr {
   return Value.Constr.at(α, new Lex.Ctr("False"), Nil.make())
}

// Used to take arbitrary value as additional argument, but now primitives have primitive arguments.
export function error (message: Value.ConstStr): (α: PersistentObject) => Value {
   return assert(false, "LambdaCalc error:\n" + message.val)
}

export function intToString (x: Value.ConstInt): (α: PersistentObject) => Value.ConstStr {
   return α => Value.ConstStr.at(α, x.toString())
}

// No longer support overloaded functions, since the demand-indexed semantics is non-trivial.
export function equalInt (x: Value.ConstInt, y: Value.ConstInt): (α: PersistentObject) => Value.Constr {
   return α => x.val === y.val ? __true(α) : __false(α)
}

export function equalStr (x: Value.ConstStr, y: Value.ConstStr): (α: PersistentObject) => Value.Constr {
   return α => x.val === y.val ? __true(α) : __false(α)
}

export function greaterInt (x: Value.ConstInt, y: Value.ConstInt): (α: PersistentObject) => Value.Constr {
   return α => x.val > y.val ? __true(α) : __false(α)
}

export function greaterStr (x: Value.ConstStr, y: Value.ConstStr): (α: PersistentObject) => Value.Constr {
   return α => x.val > y.val ? __true(α) : __false(α)
}

export function lessInt (x: Value.ConstInt, y: Value.ConstInt): (α: PersistentObject) => Value.Constr {
   return α => x.val > y.val ? __true(α) : __false(α)
}

export function lessStr (x: Value.ConstStr, y: Value.ConstStr): (α: PersistentObject) => Value.Constr {
   return α => x.val > y.val ? __true(α) : __false(α)
}

export function minus (x: Value.ConstInt, y: Value.ConstInt): (α: PersistentObject) => Value.ConstInt {
   return α => Value.ConstInt.at(α, x.val - y.val)
}

export function plus (x: Value.ConstInt, y: Value.ConstInt): (α: PersistentObject) => Value.ConstInt {
   return α => Value.ConstInt.at(α, x.val + y.val)
}

export function times (x: Value.ConstInt, y: Value.ConstInt): (α: PersistentObject) => Value.ConstInt {
   return α => Value.ConstInt.at(α, x.val * y.val)
}

export function div (x: Value.ConstInt, y: Value.ConstInt): (α: PersistentObject) => Value.ConstInt {
   // Apparently this will round in the right direction.
   return α => Value.ConstInt.at(α, ~~(x.val / y.val))
}

export function concat (x: Value.ConstStr, y: Value.ConstStr): (α: PersistentObject) => Value.ConstStr {
   return α => Value.ConstStr.at(α, x.val + y.val)
}

// Only primitive with identifiers as names are first-class, and therefore appear in the prelude.
export function prelude (): Env {
   const ρ_0: Env = Env.empty()
   let ρ: Env = Env.empty()
   unaryOps.forEach((op: UnaryOp, x: string): void => {
      ρ = ExtendEnv.make(ρ, x, EnvEntry.make(ρ_0, Nil.make(), instantiate(ρ_0)(Expr.PrimOp.at(ν(), op))))
   })
   return ρ
}
