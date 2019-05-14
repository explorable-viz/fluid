import { absurd } from "./util/Core"
import { List, cons, nil } from "./BaseTypes2"
import { Str, Constr, Value, _, make } from "./Value2"

// Environments are snoc lists.
export abstract class Env extends Constr {
}

export class EmptyEnv extends Env {
}

export function emptyEnv (): EmptyEnv {
   return make(EmptyEnv)
}

export class ExtendEnv extends Env {
   ρ: Env = _
   k: Str = _
   v: Value<any> = _
}

export function extendEnv (ρ: Env, k: Str, v: Value<any>): ExtendEnv {
   return make(ExtendEnv, ρ, k, v)
}

// Environment whose names have been projected away, leaving only list of values; cons rather than snoc, but doesn't matter.
export function entries (ρ: Env): List<Value<any>> {
   if (ρ instanceof EmptyEnv) {
      return nil()
   } else
   if (ρ instanceof ExtendEnv) {
      return cons(ρ.v, entries(ρ.ρ))
   } else {
      return absurd()
   }
}

export function get (ρ: Env, k: Str): Value<any> | undefined {
   if (ρ instanceof EmptyEnv) {
      return undefined
   } else
   if (ρ instanceof ExtendEnv) {
      if (ρ.k.val === k.val) {
         return ρ.v
      } else {
         return get(ρ.ρ, k)
      }
   } else {
      return absurd()
   }
}

export function has (ρ: Env, k: Str): boolean {
   return get(ρ, k) !== undefined
}

export function singleton (k: Str, v: Value<any>): Env {
   return extendEnv(emptyEnv(), k, v)
}

export function concat (ρ1: Env, ρ2: Env): Env {
   if (ρ2 instanceof EmptyEnv) {
      return ρ1
   } else
   if (ρ2 instanceof ExtendEnv) {
      return extendEnv(concat(ρ1, ρ2.ρ), ρ2.k, ρ2.v)
   } else {
      return absurd()
   }
}
