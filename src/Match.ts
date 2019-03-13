import { absurd } from "./util/Core"
import { Annotation, ann } from "./Annotated"
import { Cons, List, Nil, Pair } from "./BaseTypes"
import { Env } from "./Env"
import { error } from "./Eval"
import { ExplMatch, ExplVal, Match, Value } from "./ExplVal"
import { Expr, Kont } from "./Expr"

import Args = Expr.Args
import Trie = Expr.Trie

// Expose as a separate method for use by 'let'.
export function matchVar<K extends Kont<K>> (v: Value, σ: Trie.Var<K>): [Env, Match.Plug<K, Match.Var<K>>, Annotation] {
   return [Env.singleton(σ.x.str, v), Match.Plug.make(Match.Var.make(σ.x), σ.κ), ann.top]
}

export function match<K extends Kont<K>> (v: Value, σ: Trie<K>): [Env, Match.Plug<K, Match<K>>, Annotation] {
   if (Trie.Var.is(σ)) {
      return matchVar(v, σ)
   } else
   if (Trie.Constr.is(σ)) {
      if (v instanceof Value.Constr) {
         let ρ_κ_α: [Env, K, Annotation] // actually may be null, but TypeScript gets confused
         const ξ: Match<K> = Match.Constr.make(σ.cases.map((ctr_Π): Pair<string, Args<K> | Match.Args<K>> => {
            const { fst: ctr, snd: Π } = ctr_Π
            if (v.ctr.str === ctr) {
               const [ρ, {Ψ, κ}, α]: [Env, Match.Args.Plug<K, Match.Args<K>>, Annotation] = matchArgs(v.args, Π)
               ρ_κ_α = [ρ, κ, α]
               return Pair.make(ctr, Ψ)
            } else {
               return ctr_Π
            }
         }))
         if (ρ_κ_α! === undefined) { // workaround
            return error("Pattern mismatch: wrong data type.", v, σ)
         } else {
            const [ρ, κ, α]: [Env, K, Annotation] = ρ_κ_α!
            return [ρ, Match.Plug.make(ξ, κ), ann.meet(α, v.α)]
         }
      } else {
         return error("Pattern mismatch: not a data type.", v, σ)
      }
   } else {
      return absurd()
   }
}

export function unmatch<K extends Kont<K>> (ρ: Env, {ξ, κ}: Match.Plug<K, Match<K>>, α: Annotation): [Value, Trie<K>] {
   if (Match.Var.is(ξ)) {
      if (ρ.has(ξ.x.str)) {
         return [ρ.get(ξ.x.str)!, Trie.Var.make(ξ.x, κ)]
      } else {
         return absurd()
      }
   } else 
   if (Match.Constr.is(ξ)) {
      const σ: Trie<K> = Trie.Constr.make(ξ.cases.map(({ fst: ctr, snd: Π_or_Ψ }): Pair<string, Args<K>> => {
      }))
   } else {
      return absurd()
   }
}

function matchArgs<K extends Kont<K>> (tvs: List<ExplVal>, Π: Args<K>): [Env, Match.Args.Plug<K, Match.Args<K>>, Annotation] {
   // Parser ensures constructor patterns agree with constructor signatures.
   if (Cons.is(tvs) && Args.Next.is(Π)) {
      const {t, v} = tvs.head
      // codomain of ξ is Args; promote to Args | Match.Args:
      const [ρ, {ξ, κ: Πʹ}, α]: [Env, Match.Plug<Args<K>, Match<Args<K>>>, Annotation] = match(v, Π.σ),
            [ρʹ, {Ψ, κ}, αʹ]: [Env, Match.Args.Plug<K, Match.Args<K>>, Annotation] = matchArgs(tvs.tail, Πʹ)
      return [Env.concat(ρ, ρʹ), Match.Args.Plug.make(Match.Args.Next.make(ExplMatch.make(t, ξ), Ψ), κ), ann.meet(α, αʹ)]
   } else
   if (Nil.is(tvs) && Args.End.is(Π)) {
      return [Env.empty(), Match.Args.Plug.make(Match.Args.End.make<K>(), Π.κ), ann.top]
   } else {
      return absurd()
   }
}

function unmatchArgs<K extends Kont<K>> (ρ: Env, {Ψ, κ}: Match.Args.Plug<K, Match.Args<K>>, α: Annotation): [List<ExplVal>, Args<K>] {
   if (Match.Args.Next.is(Ψ)) {
      const [tus, Π]: [List<ExplVal>, Args<K>] = unmatchArgs(null, Match.Args.Plug.make(Ψ.Ψ, κ), α),
            {t, ξ} = Ψ.tξ,
            [u, σ] = unmatch(null, Match.Plug.make(ξ, Π), α)
      return [Cons.make(ExplVal.make(null, t, u), tus), Args.Next.make(σ)]
   } else
   if (Match.Args.End.is(Ψ)) {
      return [Nil.make(), Args.End.make(κ)]
   } else {
      return absurd()
   }
}
