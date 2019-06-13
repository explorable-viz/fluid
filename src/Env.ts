import { absurd } from "./util/Core"
import { DataValue } from "./DataValue"
import { Str, Value, _, make } from "./Value"
import { Versioned } from "./Versioned"

// Idiom is to permit instance methods on reflected datatypes, but not have them use polymorphism.

// Environments are snoc lists.
export abstract class Env extends DataValue<"Env"> {
   get (k: Versioned<Str>): Versioned<Value> | undefined {
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
   
   has (k: Versioned<Str>): boolean {
      return this.get(k) !== undefined
   }

   static singleton (k: Versioned<Str>, v: Versioned<Value>): Env {
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
   k: Versioned<Str> = _
   v: Versioned<Value> = _
}

export function extendEnv (ρ: Env, k: Versioned<Str>, v: Versioned<Value>): ExtendEnv {
   return make(ExtendEnv, ρ, k, v)
}
