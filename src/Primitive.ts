import { assert, make } from "./util/Core"
import { Nil } from "./BaseTypes"
import { Env, EnvEntry, ExtendEnv } from "./Env"
import { get, has } from "./FiniteMap"
import { PersistentObject, ν } from "./Runtime"
import { Expr, Lex, Trie, Value } from "./Syntax"

export type PrimResult<K> = [Value | null, K]
type TrieCtr = (body: null) => Trie.Prim<null>
type Unary<T, V> = (x: T) => (α: PersistentObject) => V
type Binary<T, U, V> = (x: T, y: U) => (α: PersistentObject) => V

function match<T extends PersistentObject | null> (v: Value, σ: Trie<T>): PrimResult<T> {
   if (v instanceof Value.ConstInt && Trie.ConstInt.is(σ)) {
      return [v, σ.body]
   } else 
   if (v instanceof Value.ConstStr && Trie.ConstStr.is(σ)) {
      return [v, σ.body]
   } else 
   if (v instanceof Value.Constr && Trie.Constr.is(σ) && has(σ.cases, v.ctr.str)) {
      return [v, get(σ.cases, v.ctr.str)!]
   } else {
      return assert(false, "Primitive demand mismatch.", v, σ)
   }
}

export class UnaryBody extends PersistentObject {
   // fields can't have polymorphic types
   op: Unary<Value, Value>

   static make<T extends Value, V extends Value> (op: Unary<T, V>): UnaryBody {
      const this_: UnaryBody = make(UnaryBody, op)
      this_.op = op
      return this_
   }

   // Access prim body via a method to reinstate the polymorphism via a cast.
   invoke<K extends PersistentObject | null> (v: Value, σ: Trie<K>): (α: PersistentObject) => PrimResult<K> {
      return α => match(this.op(v)(α), σ)
   }
} 

export class BinaryBody extends PersistentObject {
   // fields can't have polymorphic types
   op: Binary<Value, Value, Value>

   static make<T extends Value, U extends Value, V extends Value> (op: Binary<T, U, V>): BinaryBody {
      const this_: BinaryBody = make(BinaryBody, op)
      this_.op = op
      return this_
   }

   // Access prim body via a method to reinstate the polymorphism via a cast.
   invoke<K extends PersistentObject | null> (v1: Value, v2: Value, σ: Trie<K>): (α: PersistentObject) => PrimResult<K> {
      return α => match(this.op(v1, v2)(α), σ)
   }
} 

export class PrimOp extends PersistentObject {
   name: string
}

export class UnaryOp extends PrimOp {
   σ: Trie.Prim<null>
   b: UnaryBody

   static make (name: string, σ: Trie.Prim<null>, b: UnaryBody): UnaryOp {
      const this_: UnaryOp = make(UnaryOp, σ, b)
      this_.name = name
      this_.σ = σ
      this_.b = b
      return this_
   }
}

export class BinOp extends PrimOp {
   σ1: Trie.Prim<null>
   σ2: Trie.Prim<null>
   b: BinaryBody

   static make (name: string, σ1: Trie.Prim<null>, σ2: Trie.Prim<null>, b: BinaryBody): BinOp {
      const this_: BinOp = make(BinOp, σ1, σ2, b)
      this_.name = name
      this_.σ1 = σ1
      this_.σ2 = σ2
      this_.b = b
      return this_
   }
}

function makeUnary<T extends Value, V extends Value> (op: Unary<T, V>, trie: TrieCtr): UnaryOp {
   return UnaryOp.make(op.name, trie(null), UnaryBody.make(op))
}

function makeBinary<T extends Value, U extends Value, V extends Value> (op: Binary<T, U, V>, trie1: TrieCtr, trie2: TrieCtr): BinOp {
   return BinOp.make(op.name, trie1(null), trie2(null), BinaryBody.make(op))
}

const unaryOps: Map<string, UnaryOp> = new Map([
   ["error", makeUnary(error, Trie.ConstStr.make)],
   ["intToString", makeUnary(intToString, Trie.ConstInt.make)],
   ])
   
export const binaryOps: Map<string, BinOp> = new Map([
   ["-", makeBinary(minus, Trie.ConstInt.make, Trie.ConstInt.make)],
   ["+", makeBinary(plus, Trie.ConstInt.make, Trie.ConstInt.make)],
   ["*", makeBinary(times, Trie.ConstInt.make, Trie.ConstInt.make)],
   ["/", makeBinary(div, Trie.ConstInt.make, Trie.ConstInt.make)],
   ["==", makeBinary(equalInt, Trie.ConstInt.make, Trie.ConstInt.make)],
   ["===", makeBinary(equalStr, Trie.ConstStr.make, Trie.ConstStr.make)],
   [">", makeBinary(greaterInt, Trie.ConstInt.make, Trie.ConstInt.make)],
   [">>", makeBinary(greaterStr, Trie.ConstStr.make, Trie.ConstStr.make)],
   ["<", makeBinary(lessInt, Trie.ConstInt.make, Trie.ConstInt.make)],
   ["<<", makeBinary(lessStr, Trie.ConstStr.make, Trie.ConstStr.make)],
   ["++", makeBinary(concat, Trie.ConstStr.make, Trie.ConstStr.make)]
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
   let ρ: Env = Env.empty()
   unaryOps.forEach((op: UnaryOp, x: string): void => {
      ρ = ExtendEnv.make(ρ, x, EnvEntry.make(Env.empty(), Expr.EmptyRecDefs.make(), Expr.PrimOp.at(ν(), op)))
   })
   return ρ
}
