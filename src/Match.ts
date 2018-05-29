import { absurd, as, assert } from "./util/Core"
import { Cons, List, Nil, Pair } from "./BaseTypes"
import { Kont, Match, Traced, TracedMatch, Trie, Value } from "./Traced"

// The match for any evaluation with demand σ which yielded value v.
export function match (σ: Trie, v: Value | null): Match {
   if (σ instanceof Trie.Var && v === null) {
      return Match.Var.make(σ.x, σ.κ)
   } else
   if (σ instanceof Trie.Fun && v instanceof Value.Closure) {
      return Match.Fun.make(v.ρ, v.σ, σ.κ)
   } else
   if (σ instanceof Trie.ConstInt && v instanceof Value.ConstInt) {
      return Match.ConstInt.make(v.val, σ.κ)
   } else
   if (σ instanceof Trie.ConstStr && v instanceof Value.ConstStr) {
      return Match.ConstStr.make(v.val, σ.κ)
   } else
   if (σ instanceof Trie.Constr && v instanceof Value.Constr) {
      return Match.Constr.make(σ.cases.map(({ fst: ctr, snd: Π }): Pair<string, Trie.Args | Match.Args> => {
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

function matchArgs (tvs: List<Traced>): (Π: Trie.Args) => Match.Args {
   return (Π: Trie.Args): Match.Args => {
      // Parser ensures constructor patterns agree with constructor signatures.
      if (Cons.is(tvs) && Π instanceof Trie.Next) {
         const ξ: Match = match(Π.σ, tvs.head.v), 
               inj = (σ: Kont) => TracedMatch.make(null, Match.Inj.make(as(σ, Trie.Trie)))
         // codomain of ξ is another Trie.Args; promote to Match.Args:
         return Match.Next.make(TracedMatch.make(tvs.head.t, mapMatch(matchArgs(tvs.tail), inj)(ξ)))
      } else
      if (Nil.is(tvs) && Π instanceof Trie.End) {
         return Match.End.make(Π.κ)
      } else {
         return absurd()
      }
   }
}

function mapMatch (f: (κ: Kont) => Kont, g: (κ: Kont) => Kont): (ξ: Match) => Match {
   return (ξ: Match): Match => {
      if (ξ instanceof Match.ConstInt) {
         return Match.ConstInt.make(ξ.val, f(ξ.κ))
      } else
      if (ξ instanceof Match.ConstStr) {
         return Match.ConstStr.make(ξ.val, f(ξ.κ))
      } else
      if (ξ instanceof Match.Fun) {
         return Match.Fun.make(ξ.ρ, ξ.σ, f(ξ.κ))
      } else
      if (ξ instanceof Match.Var) {
         return Match.Var.make(ξ.x, f(ξ.κ))
      } else 
      if (ξ instanceof Match.Constr) {
         return Match.Constr.make(ξ.cases.map(({ fst: ctr, snd: Π_or_Ψ }): Pair<string, Trie.Args | Match.Args> => {
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

function mapTrie (f: (κ: Kont) => Kont): (σ: Trie.Trie) => Trie.Trie {
   return (σ: Trie.Trie): Trie.Trie => {
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
         return Trie.Constr.make(σ.cases.map(({ fst: ctr, snd: Π }): Pair<string, Trie.Args> => {
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

function mapTrieArgs (f: (κ: Kont) => Kont): (Π: Trie.Args) => Trie.Args {
   return (Π: Trie.Args): Trie.Args => {
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

function mapMatchArgs (f: (κ: Kont) => Kont, g: (κ: Kont) => Kont): (Ψ: Match.Args) => Match.Args {
   return (Ψ: Match.Args): Match.Args => {
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
