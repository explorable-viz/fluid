import { Bool, False, Int, Str, True } from "./BaseTypes"
import { __def, key, keyP } from "./Memo"
import { BinaryPrimOp, UnaryPartialPrimOp } from "./Syntax"
import { assertMessage, failWithMessage } from "./Util"

// Signatures of primitive operations.
export interface UnaryOp {
   (v: Object): Object
}

export interface BinaryOp {
   (v1: Object, v2: Object): Object
}

__def(partiallyApply)
export function partiallyApply (binOp: BinaryPrimOp, v1: Object): UnaryPartialPrimOp {
   const α: Addr = key(partiallyApply, arguments)
   return UnaryPartialPrimOp.at_(
      α,
      Str.at(keyP(α, 'name', 'v'), binOp + ' ' + v1),
      binOp,
      v1
   )
}

__def(equalOp)
export function equalOp (x: Object, y: Object): Bool {
   const α: Addr = key(equalOp, arguments)
   if (x instanceof Int && y instanceof Int) {
      return x.val === y.val ? True.at(α) : False.at(α)
   } else
   if (x instanceof Str && y instanceof Str) {
      return x.val === y.val ? True.at(α) : False.at(α)
   } else {
      return assertMessage(false, 'Can only compare two ints or two strings.', x, y)
   }
}

__def(greaterT)
export function greaterT (x: Int, y: Int): Bool {
   const α: Addr = key(greaterT, arguments)
   if (x instanceof Int && y instanceof Int) {
      return x.val > y.val ? True.at(α) : False.at(α)
   } else
   if (x instanceof Str && y instanceof Str) {
      return x.val > y.val ? True.at(α) : False.at(α)
   } else {
      return assertMessage(false, 'Can only compare two ints or two strings.', x, y)
   }
}

__def(lessT)
export function lessT (x: Int, y: Int): Bool {
   const α: Addr = key(lessT, arguments)
   if (x instanceof Int && y instanceof Int) {
      return x.val < y.val ? True.at(α) : False.at(α)
   } else
   if (x instanceof Str && y instanceof Str) {
      return x.val < y.val ? True.at(α) : False.at(α)
   } else {
      return assertMessage(false, 'Can only compare two ints or two strings.', x, y)
   }
}

__def(minus)
export function minus (x: Int, y: Int): Int {
   return Int.at(key(minus, arguments), x.val - y.val)
}

__def(plus)
export function plus (x: Int, y: Int): Int {
   return Int.at(key(plus, arguments), x.val + y.val)
}

__def(times)
export function times (x: Int, y: Int): Int {
   return Int.at(key(times, arguments), x.val * y.val)
}

__def(div)
export function div (x: Int, y: Int): Int {
   // Apparently this will round in the right direction.
   return Int.at(key(div, arguments), ~~(x.val / y.val))
}

__def(concat)
export function concat (x: Str, y: Str): Str {
   return Str.at(key(concat, arguments), x.val + y.val)
}

// See 0.2.4 release notes re. primitive ops with identifiers as names.
__def(error)
export function error (x: Object, message: Str): never {
   return failWithMessage("LambdaCalc error:\n" + message.val, x)
}

__def(intToString)
export function intToString (x: Int): Str {
   return Str.at(key(intToString, arguments), x.toString())
}
