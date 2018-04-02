import { make } from "./util/Core"
import { Id } from "./Memo"
import { Expr, Value } from "./Syntax"

export class EnvId extends Id {
}

export class EmptyEnvId extends EnvId { 
   static make (): EnvId {
      return make(EnvId)
   }
}

export class ExtendEnvId extends EnvId {
   j: EnvId
   k: Value.ValId

   static make (j: EnvId, k: Value.ValId): ExtendEnvId {
      return make(ExtendEnvId, j, k)
   }
}

// Environments are snoc lists. The identity of an evaluated term is the identity of the original expression
// paired with the identity of all environment entries used to close the term, in the order in which they 
// were bound. This makes identity insensitive to the choice of names, and is essentially the same as the
// approach I used in my thesis ("translating" every function body by the identity of the argument used to
// close it). This is *not* the same as hash-consing environments (which would consider the keys as well).

export type Env = [string, EnvEntry][]

export class EnvEntry {
   ρ: Env
   δ: Expr.RecDefinition[]
   e: Expr.Expr

   constructor(ρ: Env, δ: Expr.RecDefinition[], e: Expr.Expr) {
      this.ρ = ρ
      this.δ = δ
      this.e = e
   }
}

export function has (ρ: Env, k: string): boolean {
   return get(ρ, k) !== undefined
}

export function get (ρ: Env, k: string): EnvEntry | undefined {
   for (let n: number = ρ.length - 1; n >= 0; --n) {
      const [kʹ, v]: [string, EnvEntry] = ρ[n]
      if (k === kʹ) {
         return v
      }
   }
   return undefined
}
