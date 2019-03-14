import { absurd } from "./util/Core"
import { asVersioned } from "./util/Persistent"
import { Annotation, ann } from "./Annotated"
import { Cons, List, Nil, Pair } from "./BaseTypes"
import { Env } from "./Env"
import { ExplVal, Match, Value, explMatch, explVal } from "./ExplVal"
import { ValId, error } from "./Eval"
import { Expr, Kont } from "./Expr"

import Args = Expr.Args
import Trie = Expr.Trie

// Expose as a separate method for use by 'let'.
export function matchVar<K extends Kont<K>> (v: Value, σ: Trie.Var<K>): [Env, Match.Plug<K, Match.Var<K>>, Annotation] {
   return [Env.singleton(σ.x.str, v), Match.plug(Match.var_(σ.x, v), σ.κ), ann.top]
}

export function match<K extends Kont<K>> (v: Value, σ: Trie<K>): [Env, Match.Plug<K, Match<K>>, Annotation] {
   if (Trie.Var.is(σ)) {
      return matchVar(v, σ)
   } else
   if (Trie.Constr.is(σ)) {
      if (v instanceof Value.Constr) {
         let ρ_κ_α: [Env, K, Annotation] // actually may be null, but TypeScript confused
         const ξ: Match<K> = Match.constr(σ.cases.map(({ fst: ctr, snd: Π }): Pair<string, Args<K> | Match.Args<K>> => {
            if (v.ctr.str === ctr) {
               const [ρ, {Ψ, κ}, α] = matchArgs(v.args, Π)
               ρ_κ_α = [ρ, κ, α]
               return Pair.make(ctr, Ψ)
            } else {
               return Pair.make(ctr, Π)
            }
         }), v) // store v as well to provide location for unmatch
         if (ρ_κ_α! === undefined) {
            return error("Pattern mismatch: wrong data type.", v, σ)
         } else {
            const [ρ, κ, α]: [Env, K, Annotation] = ρ_κ_α!
            return [ρ, Match.plug(ξ, κ), ann.meet(α, v.α)]
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
      let tus: List<ExplVal> // actually may be null, but TypeScript assigns type "never"
      const σ: Trie<K> = Trie.Constr.make(ξ.cases.map(({ fst: ctr, snd: Π_or_Ψ }): Pair<string, Args<K>> => {
         if (Π_or_Ψ instanceof Match.Args.Args) {
            const [tusʹ, Π]: [List<ExplVal>, Args<K>] = unmatchArgs(ρ, Match.Args.plug(Π_or_Ψ, κ), α)
            tus = tusʹ
            return Pair.make(ctr, Π)
         } else
         if (Π_or_Ψ instanceof Args.Args) {
            const Π_or_Ψʹ: Args.Args<K> = Π_or_Ψ  // recover type lost by instanceof
            // TODO: mapArgs to set annotations to bot
            return Pair.make(ctr, Trie.mapArgs((k: K): K => k, Π_or_Ψʹ))
         } else {
            return absurd()
         }
      }))
      if (tus! === undefined) {
         return absurd()
      } else {
         // use the cached matched value to extract target address, and also to avoid recreating the constructor
         const k: ValId = asVersioned(ξ.v).__id as ValId
         return [Value.Constr.at(k, α, ξ.v.ctr, tus!), σ]
      }
   } else {
      return absurd()
   }
}

function matchArgs<K extends Kont<K>> (tvs: List<ExplVal>, Π: Args<K>): [Env, Match.Args.Plug<K, Match.Args<K>>, Annotation] {
   if (Cons.is(tvs) && Args.Next.is(Π)) {
      const {t, v} = tvs.head
      // codomain of ξ is Args; promote to Args | Match.Args:
      const [ρ, {ξ, κ: Πʹ}, α] = match(v, Π.σ),
            [ρʹ, {Ψ, κ}, αʹ] = matchArgs(tvs.tail, Πʹ)
      return [Env.concat(ρ, ρʹ), Match.Args.plug(Match.Args.next(explMatch(t, ξ), Ψ), κ), ann.meet(α, αʹ)]
   } else
   if (Nil.is(tvs) && Args.End.is(Π)) {
      return [Env.empty(), Match.Args.plug(Match.Args.end<K>(), Π.κ), ann.top]
   } else {
      return absurd()
   }
}

function unmatchArgs<K extends Kont<K>> (ρ: Env, {Ψ, κ}: Match.Args.Plug<K, Match.Args<K>>, α: Annotation): [List<ExplVal>, Args<K>] {
   if (Match.Args.Next.is(Ψ)) {
      const [tus, Π]: [List<ExplVal>, Args<K>] = unmatchArgs(Ψ.Ψ.ρ, Match.Args.plug(Ψ.Ψ, κ), α),
            {t, ξ} = Ψ.tξ,
            [u, σ] = unmatch(ξ.ρ, Match.plug(ξ, Π), α)
      return [Cons.make(explVal(Env.concat(ξ.ρ, Ψ.Ψ.ρ), t, u), tus), Args.Next.make(σ)]
   } else
   if (Match.Args.End.is(Ψ)) {
      return [Nil.make(), Args.End.make(κ)]
   } else {
      return absurd()
   }
}
