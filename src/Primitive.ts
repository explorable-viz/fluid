import { assert } from "./util/Core"
import { __def, addr, key, keyP } from "./Memo"
import { ν } from "./Runtime"
import { Lex, Trie, Value } from "./Syntax"

export type PrimResult<T> = [Value.Value | null, T] // v, σv
export type PrimBody<T> = (v: Value.Value | null, σ: Trie.Trie<T>) => PrimResult<T>

type Unary<T, V> = (x: T) => V
type Binary<T, U, V> = (x: T, y: U) => V
type Prim<T, U> = (x: T, σ: Trie.Trie<U>) => PrimResult<U>

// Note: primitives currently use a custom memoisation policy, although other approaches are possible.

function unaryIntStr<T> (op: Unary<Value.ConstInt, Value.ConstStr>): Prim<Value.ConstInt, T> {
   return function (x: Value.ConstInt, σ: Trie.Trie<T>): PrimResult<T> {
      if (σ instanceof Trie.ConstStr) {
         const v: Value.ConstStr = Value.ConstStr.at(key(intToString, arguments), x.toString())
         return [v, σ.body]
      } else {
         return assert(false, "Demand mismatch.")
      }
   }
}

function binaryIntIntInt<T> (op: Binary<Value.ConstInt, Value.ConstInt, Value.ConstInt>): Value.PrimOp {
   const α: Addr = addr(op)
   function first (x: Value.ConstInt, σ: Trie.Trie<T>): PrimResult<T> {
      function second (y: Value.ConstInt, τ: Trie.Trie<any>): PrimResult<any> {
         const v: Value.ConstInt = op(x, y)
         if (τ instanceof Trie.ConstInt) {
            return [v, τ.body]
         } else {
            return assert(false, "Demand mismatch.")
         }
      }
      if (σ instanceof Trie.Fun) {
         const β: Addr = keyP(α, addr(x)),
               v: Value.PrimOp = Value.PrimOp.at(β, op.name + " " + x, Trie.ConstInt.at(keyP(β, "σ"), second))
         return [v, σ.body]
      } else {
         return assert(false, "Demand mismatch.")
      }
   }
   return Value.PrimOp.at(α, name, Trie.ConstInt.at(keyP(α, "σ"), first))
}

function binaryIntIntBool<T> (op: Binary<Value.ConstInt, Value.ConstInt, Value.Constr>): Prim<Value.ConstInt, T> {
   return function (x: Value.ConstInt, σ: Trie.Trie<T>): PrimResult<T> {
      function burble (y: Value.ConstInt, τ: Trie.Trie<any>): PrimResult<any> {
         const v: Value.Constr = op(x, y)
         if (τ instanceof Trie.Constr && τ.cases.has(v.ctr.str)) {
            return [v, τ.cases.get(v.ctr.str)]
         } else {
            return assert(false, "Demand mismatch.")
         }
      }
      if (σ instanceof Trie.Fun) {
         const σʹ: Trie.ConstInt<PrimBody<any>> = Trie.ConstInt.at("", burble),
               v: Value.PrimOp = Value.PrimOp.at(key(intToString, arguments), "minus" + " " + x, σʹ)
         return [v, σ.body]
      } else {
         return assert(false, "Demand mismatch.")
      }
   }
}

// Fake "syntax" for primitive ops; might revisit.
export const ops: Value.PrimOp[] = [
   Value.PrimOp.at(ν(), "intToString", Trie.ConstInt.at(ν(), unaryIntStr(intToString))),
   binaryIntIntInt(minus),
   binaryIntIntInt(plus),
   binaryIntIntInt(times),
   binaryIntIntInt(div),
   Value.PrimOp.at(ν(), "equalInt", Trie.ConstInt.at(ν(), binaryIntIntBool(equalInt))),
   Value.PrimOp.at(ν(), "greaterInt", Trie.ConstInt.at(ν(), binaryIntIntBool(greaterInt))),
   Value.PrimOp.at(ν(), "lessInt", Trie.ConstInt.at(ν(), binaryIntIntBool(lessInt)))
]

function __true (α: Addr): Value.Constr {
   return Value.Constr.at(α, new Lex.Ctr("True"), [])
}

function __false (α: Addr): Value.Constr {
   return Value.Constr.at(α, new Lex.Ctr("False"), [])
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

// See 0.2.4 release notes re. primitive ops with identifiers as names.
__def(error)
export function error (x: Object, message: Value.ConstStr): never {
   return assert(false, "LambdaCalc error:\n" + message.val, x) as never
}

__def(intToString)
export function intToString (x: Value.ConstInt): Value.ConstStr {
   return Value.ConstStr.at(key(intToString, arguments), x.toString())
}
