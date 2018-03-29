import { assert } from "./util/Core"
import { __def, addr, key, keyP } from "./Memo"
import { Lex, Trie, Value } from "./Syntax"

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
   return makePrim(addr(op), op.name, op, at1)
}

function binary<T extends Value.Value, U extends Value.Value, V extends Value.Value> (
   op: (x: T, y: U) => V,
   at1: TrieCtr<Value.PrimOp>,
   at2: TrieCtr<V>
): Value.PrimOp {
   const α: Addr = addr(op),
         op_x: (x: T) => Value.PrimOp = // op partially applied to x 
            (x: T) => makePrim(keyP(α, addr(x)), op.name + " " + x, (y: U) => op(x, y), at2)
   return makePrim(α, op.name, op_x, at1)
}

export const ops: Value.PrimOp[] = [
   unary(error, Trie.ConstStr.at),
   unary(intToString, Trie.ConstInt.at),
   binary(minus, Trie.ConstInt.at, Trie.ConstInt.at),
   binary(plus, Trie.ConstInt.at, Trie.ConstInt.at),
   binary(times, Trie.ConstInt.at, Trie.ConstInt.at),
   binary(div, Trie.ConstInt.at, Trie.ConstInt.at),
   binary(equalInt, Trie.ConstInt.at, Trie.ConstInt.at),
   binary(equalInt, Trie.ConstStr.at, Trie.ConstStr.at),
   binary(greaterInt, Trie.ConstInt.at, Trie.ConstInt.at),
   binary(greaterInt, Trie.ConstStr.at, Trie.ConstStr.at),
   binary(lessInt, Trie.ConstInt.at, Trie.ConstInt.at),
   binary(lessInt, Trie.ConstStr.at, Trie.ConstStr.at),
   binary(concat, Trie.ConstStr.at, Trie.ConstStr.at),
]

function __true (α: Addr): Value.Constr {
   return Value.Constr.at(α, new Lex.Ctr("True"), [])
}

function __false (α: Addr): Value.Constr {
   return Value.Constr.at(α, new Lex.Ctr("False"), [])
}

// See 0.2.4 release notes re. primitive ops with identifiers as names.
// Used to take an arbitrary value as an additional argument but now primitives must have
// primitive arguments.
__def(error)
export function error (message: Value.ConstStr): Value.Value {
   return assert(false, "LambdaCalc error:\n" + message.val)
}

__def(intToString)
export function intToString (x: Value.ConstInt): Value.ConstStr {
   return Value.ConstStr.at(key(intToString, arguments), x.toString())
}

// No longer support overloaded functions, since the demand-indexed semantics is non-trivial.
__def(equalInt)
export function equalInt (x: Value.ConstInt, y: Value.ConstInt): Value.Constr {
   const α: Addr = key(equalInt, arguments)
   return x.val === y.val ? __true(α) : __false(α)
}

__def(equalStr)
export function equalStr (x: Value.ConstStr, y: Value.ConstStr): Value.Constr {
   const α: Addr = key(equalStr, arguments)
   return x.val === y.val ? __true(α) : __false(α)
}

__def(greaterInt)
export function greaterInt (x: Value.ConstInt, y: Value.ConstInt): Value.Constr {
   const α: Addr = key(greaterInt, arguments)
   return x.val > y.val ? __true(α) : __false(α)
}

__def(greaterStr)
export function greaterStr (x: Value.ConstStr, y: Value.ConstStr): Value.Constr {
   const α: Addr = key(greaterStr, arguments)
   return x.val > y.val ? __true(α) : __false(α)
}

__def(lessInt)
export function lessInt (x: Value.ConstInt, y: Value.ConstInt): Value.Constr {
   const α: Addr = key(lessInt, arguments)
   return x.val > y.val ? __true(α) : __false(α)
}

__def(lessStr)
export function lessStr (x: Value.ConstStr, y: Value.ConstStr): Value.Constr {
   const α: Addr = key(lessStr, arguments)
   return x.val > y.val ? __true(α) : __false(α)
}

__def(minus)
export function minus (x: Value.ConstInt, y: Value.ConstInt): Value.ConstInt {
   return Value.ConstInt.at(key(minus, arguments), x.val - y.val)
}

__def(plus)
export function plus (x: Value.ConstInt, y: Value.ConstInt): Value.ConstInt {
   return Value.ConstInt.at(key(plus, arguments), x.val + y.val)
}

__def(times)
export function times (x: Value.ConstInt, y: Value.ConstInt): Value.ConstInt {
   return Value.ConstInt.at(key(times, arguments), x.val * y.val)
}

__def(div)
export function div (x: Value.ConstInt, y: Value.ConstInt): Value.ConstInt {
   // Apparently this will round in the right direction.
   return Value.ConstInt.at(key(div, arguments), ~~(x.val / y.val))
}

__def(concat)
export function concat (x: Value.ConstStr, y: Value.ConstStr): Value.ConstStr {
   return Value.ConstStr.at(key(concat, arguments), x.val + y.val)
}
