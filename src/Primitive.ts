import { assert } from "./util/Core"
import { __def, addr, key, keyP } from "./Memo"
import { Lex, Trie, Value } from "./Syntax"

export type PrimResult<T> = [Value.Value | null, T] // v, σv
export type PrimBody<T = any> = (v: Value.Value | null, σ: Trie.Trie<T>) => PrimResult<T>

type Unary<T, V> = (x: T) => V
type Binary<T, U, V> = (x: T, y: U) => V

function match<T> (v: Value.Value, σ: Trie.Trie<T>): PrimResult<T> {
   if (v instanceof Value.PrimOp && σ instanceof Trie.Fun) {
      return [v, σ.body]
   }  else
   if (v instanceof Value.ConstInt && σ instanceof Trie.ConstInt) {
      return [v, σ.body]
   } else 
   if (v instanceof Value.ConstStr && σ instanceof Trie.ConstStr) {
      return [v, σ.body]
   } else 
   if (v instanceof Value.Constr && σ instanceof Trie.Constr && σ.cases.has(v.ctr.str)) {
      return σ.cases.get(v.ctr.str)
   } else {
      return assert(false, "Demand mismatch.")
   }
}

// Primitives currently use a custom memoisation policy, although other approaches are possible.
function unary<T extends Value.Value, V extends Value.Value> (
   op: Unary<T, V>,
   at1: (α: Addr, body: PrimBody) => Trie.Prim<PrimBody>,
): Value.PrimOp {
   const α: Addr = addr(op)
   return Value.PrimOp.at(α, op.name, at1(keyP(α, "σ"), (x: T, τ: Trie.Trie<T>) => match(op(x), τ)))
}

function binary<T extends Value.Value, U extends Value.Value, V extends Value.Value> (
   op: Binary<T, U, V>,
   at1: (α: Addr, body: PrimBody) => Trie.Prim<PrimBody>,
   at2: (α: Addr, body: PrimBody) => Trie.Prim<PrimBody>
): Value.PrimOp {
   const α: Addr = addr(op)
   function op_arg (x: T): Value.PrimOp {
      const β: Addr = keyP(α, addr(x))
      return Value.PrimOp.at(β, op.name + " " + x, at2(keyP(β, "σ"), (y: U, τ: Trie.Trie<T>) => match(op(x, y), τ)))
   }
   return Value.PrimOp.at(α, op.name, at1(keyP(α, "σ"), (x: T, σ: Trie.Trie<any>) => match(op_arg(x), σ)))
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
