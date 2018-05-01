import { absurd, assert, make } from "./util/Core"
import { Nil } from "./BaseTypes"
import { Env, EnvEntry, ExtendEnv } from "./Env"
import { get, has } from "./FiniteMap"
import { instantiate } from "./Instantiate"
import { PersistentObject, ν } from "./Runtime"
import { Expr, Kont, Lex, Trie, Value } from "./Syntax"

export type PrimResult = [Value | null, Kont]
type TrieCtr = (body: null) => Trie.Prim
type Unary<T, V> = (x: T) => (α: PersistentObject) => V
type Binary<T, U, V> = (x: T, y: U) => (α: PersistentObject) => V

// Parser guarantees that values/patterns respect constructor signatures.
function match (v: Value, σ: Trie): PrimResult {
   if (v instanceof Value.PrimOp && σ instanceof Trie.Fun) {
      return [v, σ.body]
   } else 
   if (v instanceof Value.ConstInt && σ instanceof Trie.ConstInt) {
      return [v, σ.body]
   } else 
   if (v instanceof Value.ConstStr && σ instanceof Trie.ConstStr) {
      return [v, σ.body]
   } else 
   if (v instanceof Value.Constr && σ instanceof Trie.Constr && has(σ.cases, v.ctr.str)) {
      const κ: Kont = get(σ.cases, v.ctr.str)!
      assert(v.args.length === 0, "Primitives must return nullary values.")
      if (κ instanceof Trie.Trie) {
         return absurd()
      } else {
         return [v, κ]
      }
   } else {
      return assert(false, "Primitive demand mismatch.", v, σ)
   }
}

// In the following two classes, we store the operation without generic type parameters, as fields can't
// have polymorphic type. Then access the operation via a method and reinstate the polymorphism via a cast.

export class UnaryBody extends PersistentObject {
   op: Unary<Value, Value>

   static make<T extends Value, V extends Value> (op: Unary<T, V>): UnaryBody {
      const this_: UnaryBody = make(UnaryBody, op)
      this_.op = op
      return this_
   }

   invoke (v: Value, σ: Trie): (α: PersistentObject) => PrimResult {
      return α => match(this.op(v)(α), σ)
   }
} 

export class BinaryBody extends PersistentObject {
   op: Binary<Value, Value, Value>

   static make<T extends Value, U extends Value, V extends Value> (op: Binary<T, U, V>): BinaryBody {
      const this_: BinaryBody = make(BinaryBody, op)
      this_.op = op
      return this_
   }

   invoke (v1: Value, v2: Value, σ: Trie): (α: PersistentObject) => PrimResult {
      return α => match(this.op(v1, v2)(α), σ)
   }
} 

export class PrimOp extends PersistentObject {
   name: string
}

export class UnaryOp extends PrimOp {
   σ: Trie.Prim
   b: UnaryBody

   static make (name: string, σ: Trie.Prim, b: UnaryBody): UnaryOp {
      const this_: UnaryOp = make(UnaryOp, σ, b)
      this_.name = name
      this_.σ = σ
      this_.b = b
      return this_
   }

   static make_<T extends Value, V extends Value> (op: Unary<T, V>, trie: TrieCtr): UnaryOp {
      return UnaryOp.make(op.name, trie(null), UnaryBody.make(op))
   }
}

export class BinaryOp extends PrimOp {
   σ1: Trie.Prim
   σ2: Trie.Prim
   b: BinaryBody

   static make (name: string, σ1: Trie.Prim, σ2: Trie.Prim, b: BinaryBody): BinaryOp {
      const this_: BinaryOp = make(BinaryOp, σ1, σ2, b)
      this_.name = name
      this_.σ1 = σ1
      this_.σ2 = σ2
      this_.b = b
      return this_
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

// Only primitive with identifiers as names are first-class and therefore appear in the prelude.
export function prelude (): Env {
   const ρ_0: Env = Env.empty()
   let ρ: Env = Env.empty()
   unaryOps.forEach((op: UnaryOp, x: string): void => {
      ρ = ExtendEnv.make(ρ, x, EnvEntry.make(ρ_0, Nil.make(), instantiate(ρ_0)(Expr.PrimOp.at(ν(), op))))
   })
   return ρ
}
