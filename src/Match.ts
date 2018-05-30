import { absurd, assert } from "./util/Core"
import { Cons, List, Nil, Pair } from "./BaseTypes"
import { Match, Traced, TracedMatch, Trie, Value } from "./Traced"

// The match for any evaluation with demand σ which yielded value v.
export function match<K> (σ: Trie<K>, v: Value | null): Match<K> {
   if (σ instanceof Trie.Var) {
      // in general v is not null, even though the demand is null
      return Match.Var.make(σ.x, v, σ.κ)
   } else
   if (σ instanceof Trie.Fun && (v instanceof Value.Closure || v instanceof Value.PrimOp)) {
      return Match.Fun.make(v, σ.κ)
   } else
   if (σ instanceof Trie.ConstInt && v instanceof Value.ConstInt) {
      return Match.ConstInt.make(v.val, σ.κ)
   } else
   if (σ instanceof Trie.ConstStr && v instanceof Value.ConstStr) {
      return Match.ConstStr.make(v.val, σ.κ)
   } else
   if (σ instanceof Trie.Constr && v instanceof Value.Constr) {
      return Match.Constr.make(σ.cases.map(({ fst: ctr, snd: Π }): Pair<string, Trie.Args<K> | Match.Args<K>> => {
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

function matchArgs<K> (tvs: List<Traced>): (Π: Trie.Args<K>) => Trie.Args<K> | Match.Args<K> {
   return (Π: Trie.Args<K>): Match.Args<K> => {
      // Parser ensures constructor patterns agree with constructor signatures.
      if (Cons.is(tvs) && Π instanceof Trie.Next) {
         const ξ: Match<K> = match(Π.σ, tvs.head.v), 
               inj = (Π: Trie.Args<K>): Trie.Args<K> => Π
         // codomain of ξ is a Trie.Args; promote to Trie.Args | Match.Args:
         return Match.Next.make(TracedMatch.make(tvs.head.t, mapMatch(matchArgs(tvs.tail), inj)(ξ)))
      } else
      if (Nil.is(tvs) && Π instanceof Trie.End) {
         return Match.End.make(Π.κ)
      } else {
         return absurd()
      }
   }
}

function mapMatch<K, Kʹ> (f: (κ: K) => Kʹ, g: (κ: K) => Kʹ): (ξ: Match<K>) => Match<Kʹ> {
   return (ξ: Match<K>): Match<Kʹ> => {
      if (ξ instanceof Match.ConstInt) {
         return Match.ConstInt.make(ξ.val, f(ξ.κ))
      } else
      if (ξ instanceof Match.ConstStr) {
         return Match.ConstStr.make(ξ.val, f(ξ.κ))
      } else
      if (ξ instanceof Match.Fun) {
         return Match.Fun.make(ξ.f, f(ξ.κ))
      } else
      if (ξ instanceof Match.Var) {
         return Match.Var.make(ξ.x, ξ.v, f(ξ.κ))
      } else 
      if (ξ instanceof Match.Constr) {
         return Match.Constr.make(ξ.cases.map(({ fst: ctr, snd: Π_or_Ψ }): Pair<string, Trie.Args<Kʹ> | Match.Args<Kʹ>> => {
            if (Π_or_Ψ instanceof Match.Args) {
               return Pair.make(ctr, mapMatchArgs(f, g)(Π_or_Ψ))
            } else
            if (Π_or_Ψ instanceof Trie.Args) {
               return Pair.make(ctr, mapTrieArgs(g)(Π_or_Ψ))
            } else {
               return absurd()
            }
         }))
      } else {
         return absurd()
      }
   }
}

function mapTrie<K, Kʹ> (f: (κ: K) => Kʹ): (σ: Trie.Trie<K>) => Trie.Trie<Kʹ> {
   return (σ: Trie.Trie<K>): Trie.Trie<Kʹ> => {
      if (σ instanceof Trie.ConstInt) {
         return Trie.ConstInt.make(f(σ.κ))
      } else
      if (σ instanceof Trie.ConstStr) {
         return Trie.ConstStr.make(f(σ.κ))
      } else
      if (σ instanceof Trie.Fun) {
         return Trie.Fun.make(f(σ.κ))
      } else
      if (σ instanceof Trie.Var) {
         return Trie.Var.make(σ.x, f(σ.κ))
      } else 
      if (σ instanceof Trie.Constr) {
         return Trie.Constr.make(σ.cases.map(({ fst: ctr, snd: Π }): Pair<string, Trie.Args<Kʹ>> => {
            if (Π instanceof Trie.Args) {
               return Pair.make(ctr, mapTrieArgs(f)(Π))
            } else {
               return absurd()
            }
         }))
      } else {
         return absurd()
      }
   }
}

function mapTrieArgs<K, Kʹ> (f: (κ: K) => Kʹ): (Π: Trie.Args<K>) => Trie.Args<Kʹ> {
   return (Π: Trie.Args<K>): Trie.Args<Kʹ> => {
      if (Π instanceof Trie.End) {
         return Trie.End.make(f(Π.κ))
      } else
      if (Π instanceof Trie.Next) {
         return Trie.Next.make(mapTrie(f)(Π.σ))
      } else {
         return absurd()
      }
   }
}

function mapMatchArgs<K, Kʹ> (f: (κ: K) => Kʹ, g: (κ: K) => Kʹ): (Ψ: Match.Args<K>) => Match.Args<Kʹ> {
   return (Ψ: Match.Args<K>): Match.Args<Kʹ> => {
      if (Ψ instanceof Match.End) {
         return Match.End.make(f(Ψ.κ))
      } else
      if (Ψ instanceof Match.Next) {
         return Match.Next.make(
            TracedMatch.make(Ψ.tξ.t,
            mapMatch(mapMatchArgs(f, g), mapMatchArgs(g, g))(Ψ.tξ.ξ)) // "bivariance"
         )
      } else {
         return absurd()
      }
   }
}
