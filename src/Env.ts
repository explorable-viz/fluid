import { absurd } from "./util/Core"
import { DataValue, ExplValue } from "./DataValue"
import { Str, _, make } from "./Value"

// Environments are snoc lists; the values are "explained" because usage information is recorded on traces.
export abstract class Env extends DataValue<"Env"> {
   get (k: Str): ExplValue | undefined {
      if (this instanceof EmptyEnv) {
         return undefined
      } else
      if (this instanceof ExtendEnv) {
         if (this.k.val === k.val) {
            return this.tv
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

   static singleton (k: Str, tv: ExplValue): ExtendEnv {
      return extendEnv(emptyEnv(), k, tv)
   }
   
   concat (ρ: Env): Env {
      if (ρ instanceof EmptyEnv) {
         return this
      } else
      if (ρ instanceof ExtendEnv) {
         return extendEnv(this.concat(ρ.ρ), ρ.k, ρ.tv)
      } else {
         return absurd()
      }
   }

   values (): ExplValue[] {
      const tvs: ExplValue[] = []
      for (let ρ: Env = this; ρ instanceof ExtendEnv; ρ = ρ.ρ) {
         tvs.push(ρ.tv)
      }
      return tvs
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
   tv: ExplValue = _
}

export function extendEnv (ρ: Env, k: Str, tv: ExplValue): ExtendEnv {
   return make(ExtendEnv, ρ, k, tv)
}
