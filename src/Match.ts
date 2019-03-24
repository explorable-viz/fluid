import { Annotation, ann } from "./util/Annotated"
import { absurd } from "./util/Core"
import { asVersioned } from "./util/Versioned"
import { Cons, List, Nil, Pair, cons, nil, pair } from "./BaseTypes"
import { Env } from "./Env"
import { ExplVal, Match, Value, explMatch, explVal } from "./ExplVal"
import { ValId, error } from "./Eval"
import { Expr, Kont } from "./Expr"
import { FiniteMap } from "./FiniteMap"

import Args = Expr.Args
import Trie = Expr.Trie

// Expose as a separate method for use by 'let'.
export function matchVar<K extends Kont<K>> (v: Value, σ: Trie.Var<K>): [Match.Plug<K, Match.Var<K>>, Annotation] {
   return [Match.plug(Match.var_(Env.singleton(σ.x.str, v), σ.x, v), σ.κ), ann.top]
}

export function match<K extends Kont<K>> (v: Value, σ: Trie<K>): [Match.Plug<K, Match<K>>, Annotation] {
   if (Trie.Var.is(σ)) {
      return matchVar(v, σ)
   } else
   if (Trie.Constr.is(σ)) {
      if (v instanceof Value.Constr) {
         let Ψκ_α: [Match.Args.Plug<K, Match.Args<K>>, Annotation] // actually may be null, but TypeScript confused
         // const Ψ: Args<K> = as(__nonNull(get(this.cases, this.v.ctr.str)), Args.Args)
         // return Ψ.ρ
         const cases: FiniteMap<string, Args<K> | Match.Args<K>> = σ.cases.map(({ fst: ctr, snd: Π }): Pair<string, Args<K> | Match.Args<K>> => {
            if (v.ctr.str === ctr) {
               const [Ψκ, α] = matchArgs(v.args, Π)
               Ψκ_α = [Ψκ, α]
               return pair(ctr, Ψκ.Ψ)
            } else {
               return pair(ctr, Π)
            }
         })
         if (Ψκ_α! === undefined) {
            return error("Pattern mismatch: wrong data type.", v, σ)
         } else {
            const [{Ψ, κ}, α] = Ψκ_α!
            // store v as well to provide location for unmatch
            return [Match.plug(Match.constr(Ψ.ρ, cases, v), κ), ann.meet(α, v.α)]
         }
      } else {
         return error("Pattern mismatch: not a data type.", v, σ)
      }
   } else {
      return absurd()
   }
}

export function unmatch<K extends Kont<K>> ({ξ, κ}: Match.Plug<K, Match<K>>, α: Annotation): [Value, Trie<K>] {
   if (Match.Var.is(ξ)) {
      if (ξ.ρ.has(ξ.x.str)) {
         return [ξ.ρ.get(ξ.x.str)!, Trie.var_(ξ.x, κ)]
      } else {
         return absurd()
      }
   } else 
   if (Match.Constr.is(ξ)) {
      let tus: List<ExplVal> // actually may be null, but TypeScript assigns type "never"
      const σ: Trie<K> = Trie.constr(ξ.cases.map(({ fst: ctr, snd: Π_or_Ψ }): Pair<string, Args<K>> => {
         if (Π_or_Ψ instanceof Match.Args.Args) {
            const [tusʹ, Π]: [List<ExplVal>, Args<K>] = unmatchArgs(Match.Args.plug(Π_or_Ψ, κ), α)
            tus = tusʹ
            return pair(ctr, Π)
         } else
         if (Π_or_Ψ instanceof Args.Args) {
            const Π_or_Ψʹ: Args.Args<K> = Π_or_Ψ  // recover type lost by instanceof
            return pair(ctr, Π_or_Ψʹ)
         } else {
            return absurd()
         }
      }))
      if (tus! === undefined) {
         return absurd()
      } else {
         // use the cached matched value to extract target address, and also to avoid recreating the constructor
         const k: ValId = asVersioned(ξ.v).__id as ValId
         return [Value.constr(k, α, ξ.v.ctr, tus!), σ]
      }
   } else {
      return absurd()
   }
}

function matchArgs<K extends Kont<K>> (tv̅: List<ExplVal>, Π: Args<K>): [Match.Args.Plug<K, Match.Args<K>>, Annotation] {
   if (Cons.is(tv̅) && Args.Next.is(Π)) {
      const {ρ, t, v} = tv̅.head
      // codomain of ξ is Args; promote to Args | Match.Args:
      const [{ξ, κ: Πʹ}, α] = match(v, Π.σ),
            [{Ψ, κ}, αʹ] = matchArgs(tv̅.tail, Πʹ)
      return [Match.Args.plug(Match.Args.next(Env.concat(ξ.ρ, Ψ.ρ), explMatch(ρ, t, ξ), Ψ), κ), ann.meet(α, αʹ)]
   } else
   if (Nil.is(tv̅) && Args.End.is(Π)) {
      return [Match.Args.plug(Match.Args.end<K>(Env.empty()), Π.κ), ann.top]
   } else {
      return absurd()
   }
}

function unmatchArgs<K extends Kont<K>> ({Ψ, κ}: Match.Args.Plug<K, Match.Args<K>>, α: Annotation): [List<ExplVal>, Args<K>] {
   if (Match.Args.Next.is(Ψ)) {
      const [tu̅, Π]: [List<ExplVal>, Args<K>] = unmatchArgs(Match.Args.plug(Ψ.Ψ, κ), α),
            {ρ, t, ξ} = Ψ.tξ,
            [u, σ] = unmatch(Match.plug(ξ, Π), α)
      return [cons(explVal(ρ, t, u), tu̅), Args.next(σ)]
   } else
   if (Match.Args.End.is(Ψ)) {
      return [nil(), Args.end(κ)]
   } else {
      return absurd()
   }
}
