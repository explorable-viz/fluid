import { __nonNull, absurd } from "./util/Core"
import { List, Pair } from "./BaseTypes"
import { Env } from "./Env"
import { Eval, EvalId, TraceId, ValId } from "./Eval"
import { Expr } from "./Expr"

import App = Expr.App
import Args = Expr.Args
import Bot = Expr.Bot
import ConstInt = Expr.ConstInt
import ConstStr = Expr.ConstStr
import Constr = Expr.Constr
import Fun = Expr.Fun
import Kont = Expr.Kont
import Let = Expr.Let
import LetRec = Expr.LetRec
import MatchAs = Expr.MatchAs
import PrimApp = Expr.PrimApp
import PrimOp = Expr.PrimOp
import RecDef = Expr.RecDef
import Trie = Expr.Trie
import Var = Expr.Var
import mapTrie = Expr.Trie.mapTrie

export function instantiate (ρ: Env, e: Expr): Expr {
   const i: TraceId<Expr> = EvalId.make(ρ.entries(), e, "trace"),
         iᵥ: ValId = EvalId.make(ρ.entries(), e, "val")
   if (e instanceof Bot) {
      return Bot.at(i)
   } else 
   if (e instanceof ConstInt) {
      return ConstInt.at(iᵥ, e.val)
   } else
   if (e instanceof ConstStr) {
      return ConstStr.at(iᵥ, e.val)
   } else
   if (e instanceof Constr) {
      // Parser ensures constructors agree with constructor signatures.
      return Constr.at(iᵥ, e.ctr, __nonNull(e.args).map(e => instantiate(ρ, e)))
   } else
   if (e instanceof Fun) {
      return Fun.at(iᵥ, instantiateTrie(ρ, e.σ))
   } else
   if (e instanceof PrimOp) {
      return PrimOp.at(iᵥ, e.op)
   } else
   if (e instanceof Var) {
      return Var.at(i, e.x)
   } else
   if (e instanceof Let) {
      return Let.at(i, instantiate(ρ, e.e), instantiateTrie(ρ, e.σ) as Trie.Var<Expr>)
   } else
   if (e instanceof LetRec) {
      const δ: List<RecDef> = e.δ.map(def => {
         const j: EvalId<RecDef, "trace"> = EvalId.make(ρ.entries(), def, "trace")
         return RecDef.at(j, def.x, instantiate(ρ, def.e))
      })
      return LetRec.at(i, δ, instantiate(Eval.closeDefs(δ, ρ, δ), e.e))
   } else
   if (e instanceof MatchAs) {
      return MatchAs.at(i, instantiate(ρ, e.e), instantiateTrie(ρ, e.σ))
   } else
   if (e instanceof App) {
      return App.at(i, instantiate(ρ, e.func), instantiate(ρ, e.arg))
   } else
   if (e instanceof PrimApp) {
      return PrimApp.at(i, instantiate(ρ, e.e1), e.opName, instantiate(ρ, e.e2))
   } else {
      return absurd()
   }
}

// See issue #33. These is some sort of heinousness to covert the continuation type.
function instantiateKont<K extends Kont<K>, Kʹ extends Kont<Kʹ>> (ρ: Env): (κ: K) => Kʹ {
   return function (κ: K): Kʹ {
      if (κ instanceof Trie.Trie) {
         return instantiateTrie<K, Kʹ>(ρ, κ) as any as Kʹ // ouch
      } else
      if (κ instanceof Expr.Expr) {
         return instantiate(ρ, κ) as any as Kʹ // also ouch
      } else {
         return absurd()
      }
   }
}

function instantiateArgs<K extends Kont<K>> (ρ: Env): (Π: Args<K>) => Args<K> {
   return function (Π: Args<K>): Args<K> {
      if (Args.End.is(Π)) {
         return Args.End.make(Π.κ)
      } else
      if (Args.Next.is(Π)) {
         return Args.Next.make(mapTrie(instantiateArgs(ρ))(instantiateTrie_(ρ, Π.σ)))
      } else {
         return absurd()
      }
   }
}

function instantiateTrie<K extends Kont<K>, Kʹ extends Kont<Kʹ>> (ρ: Env, σ: Trie<K>): Trie<Kʹ> {
   return mapTrie(instantiateKont<K, Kʹ>(ρ))(instantiateTrie_(ρ, σ))
}

function instantiateTrie_<K extends Kont<K>> (ρ: Env, σ: Trie<K>): Trie<K> {
   if (Trie.Var.is(σ)) {
      return Trie.Var.make(σ.x, σ.κ)
   } else
   if (Trie.ConstInt.is(σ)) {
      return Trie.ConstInt.make(σ.κ)
   } else
   if (Trie.ConstStr.is(σ)) {
      return Trie.ConstStr.make(σ.κ)
   } else
   if (Trie.Constr.is(σ)) {
      return Trie.Constr.make(σ.cases.map(
         ({ fst: ctr, snd: Π }: Pair<string, Args<K>>): Pair<string, Args<K>> => {
            return Pair.make(ctr, instantiateArgs(ρ)(Π))
         })
      )
   } else
   if (Trie.Fun.is(σ)) {
      return Trie.Fun.make(σ.κ)
   } else {
      return absurd()
   }
}
