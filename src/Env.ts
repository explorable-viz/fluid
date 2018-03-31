import { Expr } from "./Syntax"

// Environments are snoc lists. The identity of an evaluated term is the identity of the original expression
// paired with the identity of all values used to close the term, in the order in which they were bound. 
// This makes identity insensitive to the choice of names, and corresponds to the subsitution-based formalism.)
// The identity of environments themselves is not relevant (since way only care about the sequence of values),
// suggesting this implementation.

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

export function entries (ρ: Env): EnvEntry[] {
   return ρ.map(([x, entry]) => entry)
}
