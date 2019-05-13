import { absurd } from "./util/Core"
import { List, cons, nil } from "./BaseTypes2"
import { Value, _, make } from "./Value2"

// Func to distinguish from expression-level Fun.
export abstract class Func extends Value {
   abstract __apply (v: Value): Value
}

// Environments are snoc lists.
export abstract class Env extends Value {
}

export class EmptyEnv extends Env {
}

export function emptyEnv (): EmptyEnv {
   return make(EmptyEnv)
}

export class ExtendEnv extends Env {
   ρ: Env = _
   k: string = _
   v: Value = _
}

export function extendEnv (ρ: Env, k: string, v: Value): ExtendEnv {
   return make(ExtendEnv, ρ, k, v)
}

// Environment whose names have been projected away, leaving only list of values; cons rather than snoc, but doesn't matter.
export function entries (ρ: Env): List<Value> {
   if (ρ instanceof EmptyEnv) {
      return nil()
   } else
   if (ρ instanceof ExtendEnv) {
      return cons(ρ.v, entries(ρ.ρ))
   } else {
      return absurd()
   }
}

export function get (ρ: Env, k: string): Value | undefined {
   if (ρ instanceof EmptyEnv) {
      return undefined
   } else
   if (ρ instanceof ExtendEnv) {
      if (ρ.k === k) {
         return ρ.v
      } else {
         return get(ρ.ρ, k)
      }
   } else {
      return absurd()
   }
}

export function has (ρ: Env, k: string): boolean {
   return get(ρ, k) !== undefined
}

export function singleton (k: string, v: Value): Env {
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
