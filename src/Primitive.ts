import { assert, funName } from "./util/Core"
import { Env, EnvEntry } from "./Env"
import { Addr, keyA, keyP } from "./Memo"
import { Expr, Lex, Trie, Value } from "./Syntax"
import { ν } from "./Runtime"

export type PrimResult<T> = [Value.Value | null, T] // v, σv
export type PrimBody<T> = (v: Value.Value | null, σ: Trie.Trie<T>) => PrimResult<T>
type TrieCtr<T> = (α: Addr, body: PrimBody<T>) => Trie.Prim<PrimBody<T>>

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

function makePrim<T extends Value.Value, V extends Value.Value> (
   α: Addr, 
   name: string, 
   op: (x: T) => V,
   at1: (α: Addr, body: PrimBody<V>) => Trie.Prim<PrimBody<V>>
): Value.PrimOp {
   const primBody: PrimBody<V> = (x: T, σ: Trie.Trie<V>): PrimResult<V> => match(op(x), σ)
   return Value.PrimOp.at(α, name, at1(keyP(α, "σ"), primBody))   
}

// Primitives currently use a custom memoisation policy, although other approaches are possible.
function unary<T extends Value.Value, V extends Value.Value> (
   op: (x: T) => V,
   at1: TrieCtr<V>,
): Value.PrimOp {
   return makePrim(funName(op), op.name, op, at1)
}

function binary<T extends Value.Value, U extends Value.Value, V extends Value.Value> (
   op: (x: T, y: U) => V,
   at1: TrieCtr<Value.PrimOp>,
   at2: TrieCtr<V>
): Value.PrimOp {
   const α: Addr = funName(op),
         op_x: (x: T) => Value.PrimOp = // op partially applied to x 
            (x: T) => makePrim(keyP(α, x.__addr), op.name + " " + x, (y: U) => op(x, y), at2)
   return makePrim(α, op.name, op_x, at1)
}

function __true (α: Addr): Value.Constr {
   return Value.Constr.at(α, new Lex.Ctr("True"), [])
}

function __false (α: Addr): Value.Constr {
   return Value.Constr.at(α, new Lex.Ctr("False"), [])
}

// See 0.2.4 release notes re. primitive ops with identifiers as names.
// Used to take an arbitrary value as an additional argument but now primitives must have
// primitive arguments.
export function error (message: Value.ConstStr): Value.Value {
   return assert(false, "LambdaCalc error:\n" + message.val)
}

export function intToString (x: Value.ConstInt): Value.ConstStr {
   return Value.ConstStr.at(keyA(intToString, x), x.toString())
}

// No longer support overloaded functions, since the demand-indexed semantics is non-trivial.
export function equalInt (x: Value.ConstInt, y: Value.ConstInt): Value.Constr {
   const α: Addr = keyA(equalInt, x, y)
   return x.val === y.val ? __true(α) : __false(α)
}

export function equalStr (x: Value.ConstStr, y: Value.ConstStr): Value.Constr {
   const α: Addr = keyA(equalStr, x, y)
   return x.val === y.val ? __true(α) : __false(α)
}

export function greaterInt (x: Value.ConstInt, y: Value.ConstInt): Value.Constr {
   const α: Addr = keyA(greaterInt, x, y)
   return x.val > y.val ? __true(α) : __false(α)
}

export function greaterStr (x: Value.ConstStr, y: Value.ConstStr): Value.Constr {
   const α: Addr = keyA(greaterStr, x, y)
   return x.val > y.val ? __true(α) : __false(α)
}

export function lessInt (x: Value.ConstInt, y: Value.ConstInt): Value.Constr {
   const α: Addr = keyA(lessInt, x, y)
   return x.val > y.val ? __true(α) : __false(α)
}

export function lessStr (x: Value.ConstStr, y: Value.ConstStr): Value.Constr {
   const α: Addr = keyA(lessStr, x, y)
   return x.val > y.val ? __true(α) : __false(α)
}

export function minus (x: Value.ConstInt, y: Value.ConstInt): Value.ConstInt {
   return Value.ConstInt.at(keyA(minus, x, y), x.val - y.val)
}

export function plus (x: Value.ConstInt, y: Value.ConstInt): Value.ConstInt {
   return Value.ConstInt.at(keyA(plus, x, y), x.val + y.val)
}

export function times (x: Value.ConstInt, y: Value.ConstInt): Value.ConstInt {
   return Value.ConstInt.at(keyA(times, x, y), x.val * y.val)
}

export function div (x: Value.ConstInt, y: Value.ConstInt): Value.ConstInt {
   // Apparently this will round in the right direction.
   return Value.ConstInt.at(keyA(div, x, y), ~~(x.val / y.val))
}

export function concat (x: Value.ConstStr, y: Value.ConstStr): Value.ConstStr {
   return Value.ConstStr.at(keyA(concat, x, y), x.val + y.val)
}

// Must come after the definitions above.
export const ops: Value.PrimOp[] = [
   unary(error, Trie.ConstStr.at),
   unary(intToString, Trie.ConstInt.at),
   binary(minus, Trie.ConstInt.at, Trie.ConstInt.at),
   binary(plus, Trie.ConstInt.at, Trie.ConstInt.at),
   binary(times, Trie.ConstInt.at, Trie.ConstInt.at),
   binary(div, Trie.ConstInt.at, Trie.ConstInt.at),
   binary(equalInt, Trie.ConstInt.at, Trie.ConstInt.at),
   binary(equalStr, Trie.ConstStr.at, Trie.ConstStr.at),
   binary(greaterInt, Trie.ConstInt.at, Trie.ConstInt.at),
   binary(greaterStr, Trie.ConstStr.at, Trie.ConstStr.at),
   binary(lessInt, Trie.ConstInt.at, Trie.ConstInt.at),
   binary(lessStr, Trie.ConstStr.at, Trie.ConstStr.at),
   binary(concat, Trie.ConstStr.at, Trie.ConstStr.at),
]

// Only primitives at the moment; eventually other library code. Fake "syntax" for primitives.
export function prelude (): Env {
   let ρ: Env = []
   ops.forEach((op: Value.PrimOp): void => {
      const entry: EnvEntry = { ρ: [], δ: [], e: Expr.PrimOp.at(ν(), op) }
      ρ.push([op.name, entry])
   })
   return ρ
}
