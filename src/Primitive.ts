import { assert, funName, memo } from "./util/Core"
import { Env, EnvEntry, ExtendEnv } from "./Env"
import { ν, PersistentObject, ExternalObject } from "./Runtime"
import { Expr, Lex, Trie, Value } from "./Syntax"

export type PrimResult<T> = [Value.Value | null, T] // v, σv
export type PrimBody<T> = (v: Value.Value | null, σ: Trie.Trie<T>) => (α: PersistentObject) => PrimResult<T>
type TrieCtr<T> = (α: PersistentObject, body: PrimBody<T>) => Trie.Prim<PrimBody<T>>

function match<T> (v: Value.Value, σ: Trie.Trie<T>): PrimResult<T> {
   if (v instanceof Value.PrimOp && Trie.Fun.is(σ)) {
      return [v, σ.body]
   }  else
   if (v instanceof Value.ConstInt && Trie.ConstInt.is(σ)) {
      return [v, σ.body]
   } else 
   if (v instanceof Value.ConstStr && Trie.ConstStr.is(σ)) {
      return [v, σ.body]
   } else 
   if (v instanceof Value.Constr && Trie.Constr.is(σ) && σ.cases.has(v.ctr.str)) {
      return [v, σ.cases.get(v.ctr.str)!]
   } else {
      return assert(false, "Demand mismatch.")
   }
}

function primBody<T extends Value.Value, V extends Value.Value> (
   op: (x: T) => (α: PersistentObject) => V
): PrimBody<V> {
   return memo<PrimBody<V>>(_primBody, null, op)
}

// Needs to be memoised so a PrimBody can be contained by a PrimOp.
function _primBody<T extends Value.Value, V extends Value.Value> (
   op: (x: T) => (α: PersistentObject) => V
): PrimBody<V> {
   return (x: T, σ: Trie.Trie<V>) => (α: PersistentObject) => match(memo(op, null, x)(α), σ)
}

function makeUnary<T extends Value.Value, V extends Value.Value> (
   op: (x: T) => (α: PersistentObject) => V,
   at1: TrieCtr<V>
) {
   const α: ExternalObject = ν()
   return Value.PrimOp.at(ν(), funName(op), at1(α, primBody(op)))
}

function makeBinary<T extends Value.Value, U extends Value.Value, V extends Value.Value> (
   op: (x: T, y: U) => (α: PersistentObject) => V,
   at1: TrieCtr<Value.PrimOp>,
   at2: TrieCtr<V>
) {
   function partiallyApply (x: T): (α: PersistentObject) => Value.PrimOp {
      return (α: PersistentObject) => 
         Value.PrimOp.at(α, op.name + " " + x, at2(α, primBody((y: U) => op(x, y))))
   }
   const α: ExternalObject = ν()
   return Value.PrimOp.at(ν(), funName(op), at1(α, primBody(partiallyApply)))
}

function __true (α: PersistentObject): Value.Constr {
   return Value.Constr.at(α, new Lex.Ctr("True"), [])
}

function __false (α: PersistentObject): Value.Constr {
   return Value.Constr.at(α, new Lex.Ctr("False"), [])
}

// See 0.2.4 release notes re. primitive ops with identifiers as names.
// Used to take an arbitrary value as an additional argument but now primitives must have
// primitive arguments.
export function error (message: Value.ConstStr): (α: PersistentObject) => Value.Value {
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
   ["error", makeUnary(error, Trie.ConstStr.at)],
   ["intToString", makeUnary(intToString, Trie.ConstInt.at)],
   ["-", makeBinary(minus, Trie.ConstInt.at, Trie.ConstInt.at)],
   ["+", makeBinary(plus, Trie.ConstInt.at, Trie.ConstInt.at)],
   ["*", makeBinary(times, Trie.ConstInt.at, Trie.ConstInt.at)],
   ["/", makeBinary(div, Trie.ConstInt.at, Trie.ConstInt.at)],
   ["==", makeBinary(equalInt, Trie.ConstInt.at, Trie.ConstInt.at)],
   ["===", makeBinary(equalStr, Trie.ConstStr.at, Trie.ConstStr.at)],
   [">", makeBinary(greaterInt, Trie.ConstInt.at, Trie.ConstInt.at)],
   [">>", makeBinary(greaterStr, Trie.ConstStr.at, Trie.ConstStr.at)],
   ["<", makeBinary(lessInt, Trie.ConstInt.at, Trie.ConstInt.at)],
   ["<<", makeBinary(lessStr, Trie.ConstStr.at, Trie.ConstStr.at)],
   ["++", makeBinary(concat, Trie.ConstStr.at, Trie.ConstStr.at)]
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
