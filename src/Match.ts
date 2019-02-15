import { absurd, assert } from "./util/Core"
import { Persistent } from "./util/Persistent"
import { Cons, List, Nil, Pair } from "./BaseTypes"
import { Traced, Value } from "./Traced"

import Args = Traced.Args
import Kont = Traced.Kont
import Match = Traced.Match
import TracedMatch = Traced.TracedMatch
import Trie = Traced.Trie

// The match for any evaluation with demand σ which yielded value v.
export function match<K extends Kont<K>> (σ: Trie<K>, v: Value | null): Match<K> {
   if (Trie.Var.is(σ)) {
      // in general v is not null, even though the demand is null
      return Match.Var.make(σ.x, v, σ.κ)
   } else
   if ((v instanceof Value.Closure || v instanceof Value.PrimOp) && Trie.Fun.is(σ)) {
      return Match.Fun.make(v, σ.κ)
   } else
   if (v instanceof Value.ConstInt && Trie.ConstInt.is(σ)) {
      return Match.ConstInt.make(v.val, σ.κ)
   } else
   if (v instanceof Value.ConstStr && Trie.ConstStr.is(σ)) {
      return Match.ConstStr.make(v.val, σ.κ)
   } else
   if (v instanceof Value.Constr && Trie.Constr.is(σ)) {
      return Match.Constr.make(σ.cases.map(({ fst: ctr, snd: Π }): Pair<string, Args<K> | Match.Args<K>> => {
         if (v.ctr.str === ctr) {
            return Pair.make(ctr, matchArgs(v.args)(Π))
         } else {
            return Pair.make(ctr, Π)
         }
      }))
   } else {
      return assert(false, "Demand mismatch.", v, σ)
   }
}

function matchArgs<K extends Persistent> (tvs: List<Traced>): (Π: Args<K>) => Match.Args<K> {
   return (Π: Args<K>): Match.Args<K> => {
      // Parser ensures constructor patterns agree with constructor signatures.
      if (Cons.is(tvs) && Args.Next.is(Π)) {
         // codomain of ξ is Args; promote to Args | Match.Args:
         const ξ: Match<Args<K>> = match(Π.σ, tvs.head.v), 
               inj = (Π: Args<K>): Args<K> | Match.Args<K> => Π, 
               ξʹ = mapMatch(matchArgs(tvs.tail), inj)(ξ)
         return Match.Args.Next.make(TracedMatch.make(tvs.head.t, ξʹ))
      } else
      if (Nil.is(tvs) && (Args.End.is(Π) || Args.Top.is(Π))) {
         return Match.Args.End.make(Π.κ)
      } else {
         return absurd()
      }
   }
}

function mapMatch<K extends Persistent, Kʹ extends Persistent> (f: (κ: K) => Kʹ, g: (κ: K) => Kʹ): (ξ: Match<K>) => Match<Kʹ> {
   return (ξ: Match<K>): Match<Kʹ> => {
      if (Match.ConstInt.is(ξ)) {
         return Match.ConstInt.make(ξ.val, f(ξ.κ))
      } else
      if (Match.ConstStr.is(ξ)) {
         return Match.ConstStr.make(ξ.val, f(ξ.κ))
      } else
      if (Match.Fun.is(ξ)) {
         return Match.Fun.make(ξ.f, f(ξ.κ))
      } else
      if (Match.Var.is(ξ)) {
         return Match.Var.make(ξ.x, ξ.v, f(ξ.κ))
      } else 
      if (Match.Constr.is(ξ)) {
         return Match.Constr.make(ξ.cases.map(({ fst: ctr, snd: Π_or_Ψ }): Pair<string, Args<Kʹ> | Match.Args<Kʹ>> => {
            if (Π_or_Ψ instanceof Match.Args.Args) {
               return Pair.make(ctr, mapMatchArgs(f, g)(Π_or_Ψ))
            } else
            if (Π_or_Ψ instanceof Args.Args) {
               return Pair.make(ctr, mapArgs(g)(Π_or_Ψ))
            } else {
               return absurd()
            }
         }))
      } else {
         return absurd()
      }
   }
}

function mapArgs<K extends Persistent, Kʹ extends Persistent> (f: (κ: K) => Kʹ): (Π: Args<K>) => Args<Kʹ> {
   return (Π: Args<K>): Args<Kʹ> => {
      if (Args.End.is(Π)) {
         return Args.End.make(f(Π.κ))
      } else
      if (Args.Next.is(Π)) {
         return Args.Next.make(mapTrie(mapArgs(f))(Π.σ))
      } else {
         return absurd()
      }
   }
}

function mapMatchArgs<K extends Persistent, Kʹ extends Persistent> (f: (κ: K) => Kʹ, g: (κ: K) => Kʹ): (Ψ: Match.Args<K>) => Match.Args<Kʹ> {
   return (Ψ: Match.Args<K>): Match.Args<Kʹ> => {
      if (Match.Args.End.is(Ψ)) {
         return Match.Args.End.make(f(Ψ.κ))
      } else
      if (Match.Args.Next.is(Ψ)) {
         return Match.Args.Next.make(
            TracedMatch.make(Ψ.tξ.t,
            mapMatch(mapMatchArgs(f, g), mapMatchArgs(g, g))(Ψ.tξ.ξ)) // "bivariance"
         )
      } else {
         return absurd()
      }
   }
}

export function mapTrie<K extends Kont<K>, Kʹ extends Kont<Kʹ>> (f: (κ: K) => Kʹ): (σ: Trie.Trie<K>) => Trie.Trie<Kʹ> {
   return (σ: Trie.Trie<K>): Trie.Trie<Kʹ> => {
      if (Trie.ConstInt.is(σ)) {
         return Trie.ConstInt.make(f(σ.κ))
      } else
      if (Trie.ConstStr.is(σ)) {
         return Trie.ConstStr.make(f(σ.κ))
      } else
      if (Trie.Fun.is(σ)) {
         return Trie.Fun.make(f(σ.κ))
      } else
      if (Trie.Var.is(σ)) {
         return Trie.Var.make(σ.x, f(σ.κ))
      } else 
      if (Trie.Constr.is(σ)) {
         return Trie.Constr.make(σ.cases.map(({ fst: ctr, snd: Π }): Pair<string, Args<Kʹ>> => {
            if (Π instanceof Args.Args) {
               return Pair.make(ctr, mapArgs(f)(Π))
            } else {
               return absurd()
            }
         }))
      } else {
         return absurd()
      }
   }
}
