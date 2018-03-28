import { assert } from "./util/Core"
import { __def, key } from "./Memo"
import { Env, Lex, Trie, Value } from "./Syntax"

export type PrimResult<T> = [Value.Value | null, Env, T] // v, ρ, σv
export type PrimBody<T> = (v: Value.Value | null, σ: Trie.Trie<T>) => PrimResult<T>

function intToString2<T> (x: Value.ConstInt, σ: Trie.Trie<T>): PrimResult<T> {
   if (σ instanceof Trie.ConstStr) {
      const v: Value.ConstStr = Value.ConstStr.at(key(intToString, arguments), x.toString())
      return [v, new Map, σ.body]
   } else {
      return assert(false, "Demand mismatch.")
   }
}

function minus2<T> (x: Value.ConstInt, σ: Trie.Trie<T>): PrimResult<T> {
   function burble (y: Value.ConstInt, τ: Trie.Trie<any>): PrimResult<any> {
      if (τ instanceof Trie.ConstInt) {
         return [Value.ConstInt.at(key(minus, arguments), x.val - y.val), new Map, τ.body]
      } else {
         return assert(false, "Demand mismatch.")
      }
   }
   if (σ instanceof Trie.Fun) {
      const σʹ: Trie.ConstInt<PrimBody<any>> = Trie.ConstInt.at("", burble),
            v: Value.PrimOp = Value.PrimOp.at(key(intToString, arguments), "minus" + " " + x, σʹ)
      return [v, new Map, σ.body]
   } else {
      return assert(false, "Demand mismatch.")
   }
}

function equalOp2<T> (x: Value.ConstInt, σ: Trie.Trie<T>): PrimResult<T> {
   function burble (y: Value.ConstInt, τ: Trie.Trie<any>): PrimResult<any> {
      if (τ instanceof Trie.ConstInt) {
         const α: Addr = key(equalOp, arguments)
         return [x.val === y.val ? __true(α) : __false(α), new Map, τ.body]
      } else {
         return assert(false, "Demand mismatch.")
      }
   }
   if (σ instanceof Trie.Fun) {
      const σʹ: Trie.ConstInt<PrimBody<any>> = Trie.ConstInt.at("", burble),
            v: Value.PrimOp = Value.PrimOp.at(key(intToString, arguments), "minus" + " " + x, σʹ)
      return [v, new Map, σ.body]
   } else {
      return assert(false, "Demand mismatch.")
   }
}

export const ops: Value.PrimOp[] = [
   Value.PrimOp.at("", "intToString", Trie.ConstInt.at("", intToString2)),
   Value.PrimOp.at("", "minus", Trie.ConstInt.at("", minus2))
]

// Signatures of primitive operations.
export interface UnaryOp {
   (v: Value.Value): Value.Value
}

export interface BinaryOp {
   (v1: Value.Value, v2: Value.Value): Value.Value
}

function __true (α: Addr): Value.Constr {
   return Value.Constr.at(α, new Lex.Ctr("True"), [])
}

function __false (α: Addr): Value.Constr {
   return Value.Constr.at(α, new Lex.Ctr("False"), [])
}

/*
__def(partiallyApply)
export function partiallyApply (binOp: Value.BinaryPrimOp, v1: Value.Value): Value.UnaryPartialPrimOp {
   const α: Addr = key(partiallyApply, arguments)
   return Value.UnaryPartialPrimOp.at(α, binOp + " " + v1, binOp, v1)
}
*/

__def(equalOp)
export function equalOp (x: Value.Value, y: Value.Value): Value.Constr {
   const α: Addr = key(equalOp, arguments)
   if (x instanceof Value.ConstInt && y instanceof Value.ConstInt) {
      return x.val === y.val ? __true(α) : __false(α)
   } else
   if (x instanceof Value.ConstStr && y instanceof Value.ConstStr) {
      return x.val === y.val ? __true(α) : __false(α)
   } else {
      return assert(false, "Can only compare two ints or two strings.", x, y)
   }
}

__def(greaterT)
export function greaterT (x: Value.ConstInt, y: Value.ConstInt): Value.Constr {
   const α: Addr = key(greaterT, arguments)
   
   if (x instanceof Value.ConstInt && y instanceof Value.ConstInt) {
      return x.val > y.val ? __true(α) : __false(α)
   } else
   if (x instanceof Value.ConstStr && y instanceof Value.ConstStr) {
      return x.val > y.val ? __true(α) : __false(α)
   } else {
      return assert(false, "Can only compare two ints or two strings.", x, y)
   }
}

__def(lessT)
export function lessT (x: Value.ConstInt, y: Value.ConstInt): Value.Constr {
   const α: Addr = key(lessT, arguments)
   if (x instanceof Value.ConstInt && y instanceof Value.ConstInt) {
      return x.val < y.val ? __true(α) : __false(α)
   } else
   if (x instanceof Value.ConstStr && y instanceof Value.ConstStr) {
      return x.val < y.val ? __true(α) : __false(α)
   } else {
      return assert(false, "Can only compare two ints or two strings.", x, y)
   }
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
