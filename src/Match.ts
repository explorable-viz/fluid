import { absurd } from "./util/Core"
import { Pair } from "./BaseTypes"
import { Expr } from "./Expr"
import { Traced } from "./Traced"

import Args = Expr.Args
import Kont = Expr.Kont
import Match = Traced.Match
import TracedMatch = Traced.TracedMatch
import Trie = Expr.Trie
import mapTrie = Expr.Trie.mapTrie

function mapMatch<K extends Kont<K>, Kʹ extends Kont<Kʹ>> (f: (κ: K) => Kʹ, g: (κ: K) => Kʹ, ξ: Match<K>): Match<Kʹ> {
   if (Match.Fun.is(ξ)) {
      return Match.Fun.make(ξ.f, f(ξ.κ))
   } else
   if (Match.Var.is(ξ)) {
      return Match.Var.make(ξ.x, ξ.v, f(ξ.κ))
   } else 
   if (Match.Constr.is(ξ)) {
      return Match.Constr.make(ξ.cases.map(({ fst: ctr, snd: Π_or_Ψ }): Pair<string, Args<Kʹ> | Match.Args<Kʹ>> => {
         if (Π_or_Ψ instanceof Match.Args.Args) {
            return Pair.make(ctr, mapMatchArgs(f, g, Π_or_Ψ))
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

function mapArgs<K extends Kont<K>, Kʹ extends Kont<Kʹ>> (f: (κ: K) => Kʹ): (Π: Args<K>) => Args<Kʹ> {
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

function mapMatchArgs<K extends Kont<K>, Kʹ extends Kont<Kʹ>> (f: (κ: K) => Kʹ, g: (κ: K) => Kʹ, Ψ: Match.Args<K>): Match.Args<Kʹ> {
   if (Match.Args.End.is(Ψ)) {
      return Match.Args.End.make(f(Ψ.κ))
   } else
   if (Match.Args.Next.is(Ψ)) {
      return Match.Args.Next.make(
         TracedMatch.make(Ψ.tξ.t,
         mapMatch((Ψ: Match.Args<K>) => mapMatchArgs(f, g, Ψ), (Ψ: Match.Args<K>) => mapMatchArgs(g, g, Ψ), Ψ.tξ.ξ)) // "bivariance"
      )
   } else {
      return absurd()
   }
}

export function mapTrie2<K extends Kont<K>, Kʹ extends Kont<Kʹ>> (f: (κ: K) => Kʹ): (σ: Trie.Trie<K>) => Trie.Trie<Kʹ> {
   return (σ: Trie.Trie<K>): Trie.Trie<Kʹ> => {
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
