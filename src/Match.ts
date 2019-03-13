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
export function matchVar<K extends Kont<K>> (v: Value, σ: Trie.Var<K>): [Env, Match.Var<K>, Annotation] {
   return [Env.singleton(σ.x.str, v), Match.Var.make(σ.x, σ.κ), ann.top]
}

export function match<K extends Kont<K>> (v: Value, σ: Trie<K>): [Env, Match<K>, Annotation] {
   if (Trie.Var.is(σ)) {
      return matchVar(v, σ)
   } else
   if (Trie.Constr.is(σ)) {
      if (v instanceof Value.Constr) {
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
            return error("Pattern mismatch: wrong data type.", v, σ)
         } else {
            const [ρ, α]: [Env, Annotation] = ρ_α!
            return [ρ, ξ, ann.meet(α, v.α)]
         }
      } else {
         return error("Pattern mismatch: not a data type.", v, σ)
      }
   } else {
      return absurd()
   }
}

export function unmatch<K extends Kont<K>> (ρ: Env, ξ: Match<K>, α: Annotation): [Value, Trie<K>] {
   throw new Error("Not implemented yet")
/*
   if (Match.Var.is(ξ)) {
      if (ρ.has(ξ.x.str)) {
         return [ρ.get(ξ.x.str)!, Trie.Var.make(ξ.x, ξ.κ)]
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
*/
}

function matchArgs<K extends Kont<K>> (tvs: List<ExplVal>, Π: Args<K>): [Env, Match.Args<K>, Annotation] {
   // Parser ensures constructor patterns agree with constructor signatures.
   if (Cons.is(tvs) && Args.Next.is(Π)) {
      const {t, v} = tvs.head
      // codomain of ξ is Args; promote to Args | Match.Args:
      const [ρ, ξ, α]: [Env, Match<Args<K>>, Annotation] = match(v, Π.σ), 
            [ρʹ, Ψ, αʹ]: [Env, Match.Args<K>, Annotation] = matchArgs(tvs.tail, ξ.κ)
      return [Env.concat(ρ, ρʹ), Match.Args.Next.make(ExplMatch.make(t, ξ.setκ(Ψ.κ))), ann.meet(α, αʹ)]
   } else
   if (Nil.is(tvs) && Args.End.is(Π)) {
      return [Env.empty(), Match.Args.End.make(Π.κ), ann.top]
   } else {
      return absurd()
   }
}

/*
function unmatchArgs<K extends Kont<K>> (ρ: Env, Ψ: Match.Args<K>, α: Annotation): [List<ExplVal>, Args<K>] {
   if (Match.Args.Next.is(Ψ)) {
   } else
   if (Match.Args.End.is(Ψ)) {
      return [Nil.make(), Args.End.make(Ψ.κ)]
   } else {
      return absurd()
   }
}
*/
