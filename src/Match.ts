import { absurd } from "./util/Core"
import { Annotation, ann } from "./Annotated"
import { Cons, List, Nil, Pair } from "./BaseTypes"
import { Env } from "./Env"
import { ExplVal, Match, Value } from "./ExplVal"
import { Expr } from "./Expr"

import Args = Expr.Args
import Kont = Expr.Kont
import Trie = Expr.Trie

export function match<K extends Kont<K>> (tv: ExplVal, σ: Trie<K>): [Env, Match<K>, Annotation] {
   const v: Value = tv.v
   if (Trie.Var.is(σ)) {
      return [Env.singleton(σ.x.str, tv), Match.Var.make(σ.x, σ.κ), ann.top]
   } else
   if (v instanceof Value.Constr && Trie.Constr.is(σ)) {
      let ρ_α: [Env, Annotation] // actually may be null, but TypeScript can't handle it
      const ξ: Match<K> = Match.Constr.make(σ.cases.map(({ fst: ctr, snd: Π }): Pair<string, Args<K> | Match.Args<K>> => {
         if (v.ctr.str === ctr) {
            const [ρ, Ψ, α]: [Env, Match.Args<K>, Annotation] = matchArgs(v.args, Π)
            ρ_α = [ρ, α]
            return Pair.make(ctr, Ψ)
         } else {
            return Pair.make(ctr, Π)
         }
      }))
      if (ρ_α! === undefined) { // workaround
         return absurd("Pattern mismatch: wrong data type.", v, σ)
      } else {
         const [ρ, α]: [Env, Annotation] = ρ_α!
         return [ρ, ξ, ann.meet(α, v.α)]
      }
   } else {
      return absurd("Pattern mismatch: not a data type.", v, σ)
   }
}

export function unmatch<K extends Kont<K>> (ρ: Env, κ: K, α: Annotation): [ExplVal, Trie<K>] {
   throw new Error("Not yet implemented")
}

function matchArgs<K extends Kont<K>> (tvs: List<ExplVal>, Π: Args<K>): [Env, Match.Args<K>, Annotation] {
   // Parser ensures constructor patterns agree with constructor signatures.
   if (Cons.is(tvs) && Args.Next.is(Π)) {
      // codomain of ξ is Args; promote to Args | Match.Args:
      const [ρ, ξ, α]: [Env, Match<Args<K>>, Annotation] = match(tvs.head, Π.σ), 
            [ρʹ, κ, αʹ]: [Env, Match.Args<K>, Annotation] = matchArgs(tvs.tail, ξ.κ)
      return [Env.concat(ρ, ρʹ), κ, ann.meet(α, αʹ)]
   } else
   if (Nil.is(tvs) && Args.End.is(Π)) {
      return [Env.empty(), Match.Args.End.make(Π.κ), ann.top]
   } else {
      return absurd()
   }
}
