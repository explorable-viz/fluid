import { assert } from "./util/Core"
import { __def, key } from "./Memo"
import { BinaryPrimOp, ConstInt, Constr, ConstStr, Lex, UnaryPartialPrimOp, Value } from "./Syntax"

// Signatures of primitive operations.
export interface UnaryOp {
   (v: Value): Value
}

export interface BinaryOp {
   (v1: Value, v2: Value): Value
}

function __true (α: Addr): Constr {
   return Constr.at(α, new Lex.Ctr("True"), [])
}

function __false (α: Addr): Constr {
   return Constr.at(α, new Lex.Ctr("False"), [])
}

__def(partiallyApply)
export function partiallyApply (binOp: BinaryPrimOp, v1: Value): UnaryPartialPrimOp {
   const α: Addr = key(partiallyApply, arguments)
   return UnaryPartialPrimOp.at(
      α,
      binOp + ' ' + v1,
      binOp,
      v1
   )
}

__def(equalOp)
export function equalOp (x: Value, y: Value): Constr {
   const α: Addr = key(equalOp, arguments)
   if (x instanceof ConstInt && y instanceof ConstInt) {
      return x.val === y.val ? __true(α) : __false(α)
   } else
   if (x instanceof ConstStr && y instanceof ConstStr) {
      return x.val === y.val ? __true(α) : __false(α)
   } else {
      return assert(false, "Can only compare two ints or two strings.", x, y)
   }
}

__def(greaterT)
export function greaterT (x: ConstInt, y: ConstInt): Constr {
   const α: Addr = key(greaterT, arguments)
   
   if (x instanceof ConstInt && y instanceof ConstInt) {
      return x.val > y.val ? __true(α) : __false(α)
   } else
   if (x instanceof ConstStr && y instanceof ConstStr) {
      return x.val > y.val ? __true(α) : __false(α)
   } else {
      return assert(false, "Can only compare two ints or two strings.", x, y)
   }
}

__def(lessT)
export function lessT (x: ConstInt, y: ConstInt): Constr {
   const α: Addr = key(lessT, arguments)
   if (x instanceof ConstInt && y instanceof ConstInt) {
      return x.val < y.val ? __true(α) : __false(α)
   } else
   if (x instanceof ConstStr && y instanceof ConstStr) {
      return x.val < y.val ? __true(α) : __false(α)
   } else {
      return assert(false, "Can only compare two ints or two strings.", x, y)
   }
}

__def(minus)
export function minus (x: ConstInt, y: ConstInt): ConstInt {
   return ConstInt.at(key(minus, arguments), x.val - y.val)
}

__def(plus)
export function plus (x: ConstInt, y: ConstInt): ConstInt {
   return ConstInt.at(key(plus, arguments), x.val + y.val)
}

__def(times)
export function times (x: ConstInt, y: ConstInt): ConstInt {
   return ConstInt.at(key(times, arguments), x.val * y.val)
}

__def(div)
export function div (x: ConstInt, y: ConstInt): ConstInt {
   // Apparently this will round in the right direction.
   return ConstInt.at(key(div, arguments), ~~(x.val / y.val))
}

__def(concat)
export function concat (x: ConstStr, y: ConstStr): ConstStr {
   return ConstStr.at(key(concat, arguments), x.val + y.val)
}

// See 0.2.4 release notes re. primitive ops with identifiers as names.
__def(error)
export function error (x: Object, message: ConstStr): never {
   return <never>assert(false, "LambdaCalc error:\n" + message.val, x)
}

__def(intToString)
export function intToString (x: ConstInt): ConstStr {
   return ConstStr.at(key(intToString, arguments), x.toString())
}
