import { __nonNull, absurd } from "./util/Core"
import { List, Pair } from "./BaseTypes"
import { Env } from "./Env"
import { Eval, EvalId, TraceId, ValId } from "./Eval"
import { Expr } from "./Expr"
import { mapTrie } from "./Match"
import { Trace, Traced, Value } from "./Traced"

import App = Traced.App
import Args = Traced.Args
import Bot = Traced.Bot
import Empty = Traced.Empty
import Kont = Traced.Kont
import Let = Traced.Let
import LetRec = Traced.LetRec
import MatchAs = Traced.MatchAs
import PrimApp = Traced.PrimApp
import RecDef = Traced.RecDef
import Trie = Traced.Trie
import Var = Traced.Var

export function instantiate (ρ: Env): (e: Expr) => Traced {
   return function (e: Expr): Traced {
      const i: TraceId<Expr> = EvalId.make(ρ.entries(), e, "trace"),
            iᵥ: ValId = EvalId.make(ρ.entries(), e, "val")
      if (e instanceof Expr.Bot) {
         return Traced.make(Bot.at(i), null)
      } else 
      if (e instanceof Expr.ConstInt) {
         return Traced.make(Empty.at(i), Value.ConstInt.at(iᵥ, e.val))
      } else
      if (e instanceof Expr.ConstStr) {
         return Traced.make(Empty.at(i), Value.ConstStr.at(iᵥ, e.val))
      } else
      if (e instanceof Expr.Constr) {
         // Parser ensures constructors agree with constructor signatures.
         return Traced.make(Empty.at(i), Value.Constr.at(iᵥ, e.ctr, __nonNull(e.args).map(instantiate(ρ))))
      } else
      if (e instanceof Expr.Fun) {
         // No need to use "unknown" environment here because we have ρ.
         return Traced.make(Empty.at(i), Value.Closure.at(iᵥ, ρ, instantiateTrie(ρ, e.σ)))
      } else
      if (e instanceof Expr.PrimOp) {
         return Traced.make(Empty.at(i), Value.PrimOp.at(iᵥ, e.op))
      } else
      if (e instanceof Expr.Var) {
         return Traced.make(Var.at(i, e.x, null), null)
      } else
      if (e instanceof Expr.Let) {
         // Trace must still be null even though I know "statically" which branch will be taken.
         const t: Trace = Let.at(i, instantiate(ρ)(e.e), instantiateTrie(ρ, e.σ) as Trie.Var<Traced>, null)
         return Traced.make(t, null)
      } else
      if (e instanceof Expr.LetRec) {
         const δ: List<RecDef> = e.δ.map(def => {
            const j: EvalId<Expr.RecDef, "trace"> = EvalId.make(ρ.entries(), def, "trace")
            return RecDef.at(j, def.x, instantiate(ρ)(def.e))
         })
         const t: Trace = LetRec.at(i, δ, instantiate(Eval.closeDefs(δ, ρ, δ))(e.e))
         return Traced.make(t, null)
      } else
      if (e instanceof Expr.MatchAs) {
         return Traced.make(MatchAs.at(i, instantiate(ρ)(e.e), instantiateTrie(ρ, e.σ), null), null)
      } else
      if (e instanceof Expr.App) {
         return Traced.make(App.at(i, instantiate(ρ)(e.func), instantiate(ρ)(e.arg), null), null)
      } else
      if (e instanceof Expr.PrimApp) {
         return Traced.make(PrimApp.at(i, instantiate(ρ)(e.e1), e.opName, instantiate(ρ)(e.e2)), null)
      } else {
         return absurd()
      }
   }
}

// See issue #33. These is some sort of heinousness to covert the continuation type.
function instantiateKont<K extends Expr.Kont<K>, Kʹ extends Kont<Kʹ>> (ρ: Env): (κ: K) => Kʹ {
   return function (κ: K): Kʹ {
      if (κ instanceof Expr.Trie.Trie) {
         return instantiateTrie<K, Kʹ>(ρ, κ) as any as Kʹ // ouch
      } else
      if (κ instanceof Expr.Expr) {
         return instantiate(ρ)(κ) as any as Kʹ // also ouch
      } else {
         return absurd()
      }
   }
}

function instantiateArgs<K extends Expr.Kont<K>> (ρ: Env): (Π: Expr.Args<K>) => Args<K> {
   return function (Π: Expr.Args<K>): Args<K> {
      if (Expr.Args.End.is(Π)) {
         return Args.End.make(Π.κ)
      } else
      if (Expr.Args.Next.is(Π)) {
         return Args.Next.make(mapTrie(instantiateArgs(ρ))(instantiateTrie_(ρ, Π.σ)))
      } else {
         return absurd()
      }
   }
}

function instantiateTrie<K extends Expr.Kont<K>, Kʹ extends Kont<Kʹ>> (ρ: Env, σ: Expr.Trie<K>): Trie<Kʹ> {
   return mapTrie(instantiateKont<K, Kʹ>(ρ))(instantiateTrie_(ρ, σ))
}

function instantiateTrie_<K extends Expr.Kont<K>> (ρ: Env, σ: Expr.Trie<K>): Trie<K> {
   if (Expr.Trie.Var.is(σ)) {
      return Trie.Var.make(σ.x, σ.κ)
   } else
   if (Expr.Trie.ConstInt.is(σ)) {
      return Trie.ConstInt.make(σ.κ)
   } else
   if (Expr.Trie.ConstStr.is(σ)) {
      return Trie.ConstStr.make(σ.κ)
   } else
   if (Expr.Trie.Constr.is(σ)) {
      return Trie.Constr.make(σ.cases.map(
         ({ fst: ctr, snd: Π }: Pair<string, Expr.Args<K>>): Pair<string, Args<K>> => {
            return Pair.make(ctr, instantiateArgs(ρ)(Π))
         })
      )
   } else
   if (Expr.Trie.Fun.is(σ)) {
      return Trie.Fun.make(σ.κ)
   } else {
      return absurd()
   }
}
