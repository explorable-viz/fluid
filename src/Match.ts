import { absurd } from "./util/Core"
import { PersistentObject, make } from "./util/Persistent"
import { Annotation, ann } from "./Annotated"
import { Cons, List, Nil, Pair } from "./BaseTypes"
import { Env } from "./Env"
import { error } from "./Eval"
import { ExplVal, Match, Value, explMatch } from "./ExplVal"
import { Expr, Kont } from "./Expr"

import Args = Expr.Args
import Trie = Expr.Trie

export class MatchId implements PersistentObject {
   v: Value

   constructor_ (v: Value): void {
      this.v = v
   }
}

function matchId (v: Value): MatchId {
   return make(MatchId, v)
}

// Expose as a separate method for use by 'let'.
export function matchVar<K extends Kont<K>> (v: Value, σ: Trie.Var<K>): [Env, Match.Plug<K, Match.Var<K>>, Annotation] {
   return [Env.singleton(σ.x.str, v), Match.plug(Match.var_(matchId(v), σ.x), σ.κ), ann.top]
}

export function match<K extends Kont<K>> (v: Value, σ: Trie<K>): [Env, Match.Plug<K, Match<K>>, Annotation] {
   if (Trie.Var.is(σ)) {
      return matchVar(v, σ)
   } else
   if (Trie.Constr.is(σ)) {
      if (v instanceof Value.Constr) {
         let ρ_κ_α: [Env, K, Annotation] // actually may be null, but TypeScript gets confused
         const ξ: Match<K> = Match.constr(matchId(v), σ.cases.map(({ fst: ctr, snd: Π }): Pair<string, Args<K> | Match.Args<K>> => {
            if (v.ctr.str === ctr) {
               const [ρ, {Ψ, κ}, α] = matchArgs(v.args, Π)
               ρ_κ_α = [ρ, κ, α]
               return Pair.make(ctr, Ψ)
            } else {
               return Pair.make(ctr, Π)
            }
         }))
         if (ρ_κ_α! === undefined) { // workaround
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
   throw new Error("Not implemented yet")
/*
   if (Match.Var.is(ξ)) {
      if (ρ.has(ξ.x.str)) {
         return [ρ.get(ξ.x.str)!, Trie.Var.make(ξ.x, κ)]
      } else {
         return absurd()
      }
   } else 
   if (Match.Constr.is(ξ)) {
      const σ: Trie<K> = Trie.Constr.make(ξ.cases.map(({ fst: ctr, snd: Π_or_Ψ }): Pair<string, Args<K>> => {
         if (Π_or_Ψ instanceof Match.Args.Args) {
            const [tus, Π]: [List<ExplVal>, Args<K>] = unmatchArgs(null, Match.Args.Plug.make(Π_or_Ψ, κ), α)
            return Pair.make(ctr, Π)
         } else {
            // TODO: mapArgs to set annotations to bot
            return Pair.make(ctr, Π_or_Ψ)
         }
      }))
      return [null, σ]
   } else {
      return absurd()
   }
*/
}

function matchArgs<K extends Kont<K>> (tvs: List<ExplVal>, Π: Args<K>): [Env, Match.Args.Plug<K, Match.Args<K>>, Annotation] {
   // Parser ensures constructor patterns agree with constructor signatures.
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
/*
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
*/
