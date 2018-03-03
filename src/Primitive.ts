import { assert } from "./util/Core"
import { __def, key } from "./Memo"
import { Lex, Value } from "./Syntax"

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

__def(partiallyApply)
export function partiallyApply (binOp: Value.BinaryPrimOp, v1: Value.Value): Value.UnaryPartialPrimOp {
   const α: Addr = key(partiallyApply, arguments)
   return Value.UnaryPartialPrimOp.at(α, binOp + " " + v1, binOp, v1)
}

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
