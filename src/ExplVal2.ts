import { Env } from "./Env2"
import { Expr } from "./Expr2"
import { Func, Value, make } from "./Value2"

export class Closure extends Value {
   ρ: Env // ρ is _not_ closing for σ; need to extend with the bindings in δ
   f: Func<Expr>
}

export function closure (ρ: Env, f: Func<Expr>): Closure {
   return make(Closure, { ρ, f })
}

export namespace Expl {
   export abstract class Expl extends Value {
   }

   export class Empty extends Expl {
   }

   export function empty (): Empty {
      return make(Empty, {})
   }
}

type Expl = Expl.Expl

export type ExplVal = [Expl, Value]
