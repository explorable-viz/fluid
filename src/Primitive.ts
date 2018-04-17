import { assert, funName, memo } from "./util/Core"
import { Nil } from "./BaseTypes"
import { Env, EnvEntry, ExtendEnv } from "./Env"
import { get, has } from "./FiniteMap"
import { Persistent, PersistentObject, ν } from "./Runtime"
import { Expr, Lex, Trie, Value } from "./Syntax"

export type PrimResult<T> = [Value | null, T] // v, σv
export type PrimBody<T extends Persistent> = (v: Value | null, σ: Trie<T>) => (α: PersistentObject) => PrimResult<T>
type TrieCtr<T extends Persistent> = (body: PrimBody<T>) => Trie.Prim<PrimBody<T>>
type Unary<T, V> = (x: T) => (α: PersistentObject) => V
type Binary<T, U, V> = (x: T, y: U) => (α: PersistentObject) => V

function match<T extends Persistent> (v: Value, σ: Trie<T>): PrimResult<T> {
   if (v instanceof Value.PrimOp && Trie.Fun.is(σ)) {
      return [v, σ.body]
   }  else
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

function primBody<T extends Value, V extends Value> (op: Unary<T, V>): PrimBody<V> {
   return memo<PrimBody<V>>(_primBody, null, op)
}

// Memoise for persistent PrimBody function objects.
function _primBody<T extends Value, V extends Value> (op: Unary<T, V>): PrimBody<V> {
   return (x: T, σ: Trie<V>) => 
      (α: PersistentObject) => match(memo(op, null, x)(α), σ)
}

function makeUnary<T extends Value, V extends Value> (
   op: Unary<T, V>, trie1: TrieCtr<V>
) {
   return Value.PrimOp.at(ν(), funName(op), trie1(primBody(op)))
}

function makeBinary<T extends Value, U extends Value, V extends Value> (
   op: Binary<T, U, V>,
   trie1: TrieCtr<Value.PrimOp>,
   trie2: TrieCtr<V>
) {   
   const partialApp: Unary<T, Value.PrimOp> = 
      (x: T) => (α: PersistentObject) => 
         // memoise to obtain unique PrimBody for each partial application:
         Value.PrimOp.at(α, op.name + " " + x, trie2(primBody(memo<Unary<U, V>>(partiallyApply, null, op, x))))
   return Value.PrimOp.at(ν(), funName(op), trie1(primBody(partialApp)))
}

// Needs to be a "static" definition; can't memoise function expressions.
function partiallyApply<T, U, V> (op: Binary<T, U, V>, x: T): Unary<U, V> {
   return (y: U) => op(x, y)
}

function __true (α: PersistentObject): Value.Constr {
   return Value.Constr.at(α, new Lex.Ctr("True"), Nil.make())
}

function __false (α: PersistentObject): Value.Constr {
   return Value.Constr.at(α, new Lex.Ctr("False"), Nil.make())
}

// See 0.2.4 release notes re. primitive ops with identifiers as names.
// Used to take an arbitrary value as an additional argument but now primitives must have
// primitive arguments.
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

// Must come after the definitions above.
const ops: [string, Value.PrimOp][] = [
   ["error", makeUnary(error, Trie.ConstStr.make)],
   ["intToString", makeUnary(intToString, Trie.ConstInt.make)],
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
]

// Fake "syntax" for primitives.
export function prelude (): Env {
   let ρ: Env = Env.empty()
   ops.forEach(([x, op]: [string, Value.PrimOp]): void => {
      const e: Expr.PrimOp = Expr.PrimOp.at(ν(), op)
      ρ = ExtendEnv.make(ρ, x, EnvEntry.make(Env.empty(), Expr.EmptyRecDefs.make(), e))
   })
   return ρ
}
