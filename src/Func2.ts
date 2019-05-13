import { List } from "./BaseTypes2"
import { Env } from "./Env2"
import { Expr } from "./Expr2"
import { Value, _, make } from "./Value2"

import Trie = Expr.Trie

export class Closure extends Value {
   ρ: Env = _                 // ρ is _not_ closing for σ; need to extend with the bindings in δ
   δ: List<Expr.RecDef> = _
   σ: Trie<Expr> = _
}

export function closure (ρ: Env, δ: List<Expr.RecDef>, σ: Trie<Expr>): Closure {
   return make(Closure, ρ, δ, σ)
}

// Func to distinguish from expression-level Fun. See GitHub issue #128.
export abstract class Func<K> extends Value {
   abstract __apply (v: Value): [Env, K]
}
