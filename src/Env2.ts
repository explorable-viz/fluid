import { absurd } from "./util/Core"
import { List, cons, nil } from "./BaseTypes2"
import { Str, Constr, Value, _, make } from "./Value2"

// Idiom is to permit instance methods on reflected datatypes, but not have them use polymorphism.

// Environments are snoc lists.
export abstract class Env extends Constr {
   // Environment whose names have been projected away, leaving only list of values; cons rather than snoc, but doesn't matter.
   entries (): List<Value> {
      if (this instanceof EmptyEnv) {
         return nil()
      } else
      if (this instanceof ExtendEnv) {
         return cons(this.v, this.ρ.entries())
      } else {
         return absurd()
      }
   }

   get (k: Str): Value | undefined {
      if (this instanceof EmptyEnv) {
         return undefined
      } else
      if (this instanceof ExtendEnv) {
         if (this.k.val === k.val) {
            return this.v
         } else {
            return this.ρ.get(k)
         }
      } else {
         return absurd()
      }
   }
   
   has (k: Str): boolean {
      return this.get(k) !== undefined
   }

   static singleton (k: Str, v: Value): Env {
      return extendEnv(emptyEnv(), k, v)
   }
   
   concat (ρ: Env): Env {
      if (ρ instanceof EmptyEnv) {
         return this
      } else
      if (ρ instanceof ExtendEnv) {
         return extendEnv(this.concat(ρ.ρ), ρ.k, ρ.v)
      } else {
         return absurd()
      }
   }
}

export class EmptyEnv extends Env {
}

export function emptyEnv (): EmptyEnv {
   return make(EmptyEnv)
}

export class ExtendEnv extends Env {
   ρ: Env = _
   k: Str = _
   v: Value = _
}

export function extendEnv (ρ: Env, k: Str, v: Value): ExtendEnv {
   return make(ExtendEnv, ρ, k, v)
}
