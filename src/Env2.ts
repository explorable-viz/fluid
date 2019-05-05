import { absurd } from "./util/Core"
import { Value, _, make } from "./ExplVal2"
import { Cons, List, Nil, cons, nil } from "./BaseTypes2"

// Environments are snoc lists.

export abstract class Env implements Value {
   // Environment whose names have been projected away, leaving only list of values; cons rather than snoc, but doesn't matter.
   abstract entries (): List<Value>
   abstract get (k: string): Value | undefined

   has (k: string): boolean {
      return this.get(k) !== undefined
   }

   static singleton (k: string, v: Value): Env {
      return extendEnv(emptyEnv(), k, v)
   }

   static concat (ρ1: Env, ρ2: Env): Env {
      if (ρ2 instanceof EmptyEnv) {
         return ρ1
      } else
      if (ρ2 instanceof ExtendEnv) {
         return extendEnv(Env.concat(ρ1, ρ2.ρ), ρ2.k, ρ2.v)
      } else {
         return absurd()
      }
   }
}

export class EmptyEnv extends Env {
   entries (): Nil<Value> {
      return nil()
   }

   get (k: string): undefined {
      return undefined
   }
}

export function emptyEnv (): EmptyEnv {
   return make(EmptyEnv, {})
}

export class ExtendEnv extends Env {
   ρ: Env = _
   k: string = _
   v: Value = _

   entries (): Cons<Value> {
      return cons(this.v, this.ρ.entries())
   }

   get (k: string): Value | undefined {
      if (this.k === k) {
         return this.v
      } else {
         return this.ρ.get(k)
      }
   }
}

export function extendEnv (ρ: Env, k: string, v: Value): ExtendEnv {
   return make(ExtendEnv, { ρ, k, v })
}
