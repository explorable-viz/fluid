import { __nonNull, absurd } from "./util/Core"
import { asVersioned } from "./util/Persistent"
import { List, Pair } from "./BaseTypes"
import { Env } from "./Env"
import { ExprId } from "./Eval"
import { Expr } from "./Expr"

import App = Expr.App
import Args = Expr.Args
import BinaryApp = Expr.BinaryApp
import Bot = Expr.Bot
import ConstInt = Expr.ConstInt
import ConstStr = Expr.ConstStr
import Constr = Expr.Constr
import Fun = Expr.Fun
import Kont = Expr.Kont
import Let = Expr.Let
import LetRec = Expr.LetRec
import MatchAs = Expr.MatchAs
import PrimOp = Expr.PrimOp
import RecDef = Expr.RecDef
import Trie = Expr.Trie
import Var = Expr.Var
import mapTrie = Expr.Trie.mapTrie

export function instantiate (ρ: Env, e: Expr): Expr {
   const j: ExprId = ExprId.make(ρ.entries(), asVersioned(e))
   if (e instanceof Bot) {
      return Bot.at(j)
   } else 
   if (e instanceof ConstInt) {
      return ConstInt.at(j, e.val)
   } else
   if (e instanceof ConstStr) {
      return ConstStr.at(j, e.val)
   } else
   if (e instanceof Constr) {
      // Parser ensures constructors agree with constructor signatures.
      return Constr.at(j, e.ctr, __nonNull(e.args).map(e => instantiate(ρ, e)))
   } else
   if (e instanceof Fun) {
      return Fun.at(j, instantiateTrie(ρ, e.σ))
   } else
   if (e instanceof PrimOp) {
      return PrimOp.at(j, e.op)
   } else
   if (e instanceof Var) {
      return Var.at(j, e.x)
   } else
   if (e instanceof Let) {
      return Let.at(j, instantiate(ρ, e.e), instantiateTrie(ρ, e.σ) as Trie.Var<Expr>)
   } else
   if (e instanceof LetRec) {
      const δ: List<RecDef> = e.δ.map(def => {
         const i: ExprId = ExprId.make(ρ.entries(), asVersioned(def))
         return RecDef.at(i, def.x, instantiate(ρ, def.f) as Fun)
      })
      return LetRec.at(j, δ, instantiate(ρ, e.e))
   } else
   if (e instanceof MatchAs) {
      return MatchAs.at(j, instantiate(ρ, e.e), instantiateTrie(ρ, e.σ))
   } else
   if (e instanceof App) {
      return App.at(j, instantiate(ρ, e.func), instantiate(ρ, e.arg))
   } else
   if (e instanceof BinaryApp) {
      return BinaryApp.at(j, instantiate(ρ, e.e1), e.opName, instantiate(ρ, e.e2))
   } else {
      return absurd()
   }
}

// See issue #33. These is some sort of heinousness to covert the continuation type.
function instantiateKont<K extends Kont<K>, Kʹ extends Kont<Kʹ>> (ρ: Env, κ: K): Kʹ {
   if (κ instanceof Trie.Trie) {
      return instantiateTrie<K, Kʹ>(ρ, κ) as any as Kʹ // ouch
   } else
   if (κ instanceof Expr.Expr) {
      return instantiate(ρ, κ) as any as Kʹ // also ouch
   } else {
      return absurd()
   }
}

function instantiateArgs<K extends Kont<K>> (ρ: Env, Π: Args<K>): Args<K> {
   if (Args.End.is(Π)) {
      return Args.End.make(Π.κ)
   } else
   if (Args.Next.is(Π)) {
      return Args.Next.make(mapTrie((Π: Args<K>) => instantiateArgs(ρ, Π))(instantiateTrie_(ρ, Π.σ)))
   } else {
      return absurd()
   }
}

function instantiateTrie<K extends Kont<K>, Kʹ extends Kont<Kʹ>> (ρ: Env, σ: Trie<K>): Trie<Kʹ> {
   return mapTrie((κ: K) => instantiateKont<K, Kʹ>(ρ, κ))(instantiateTrie_(ρ, σ))
}

// This looks weird - why no instantiateKont?
function instantiateTrie_<K extends Kont<K>> (ρ: Env, σ: Trie<K>): Trie<K> {
   if (Trie.Var.is(σ)) {
      return Trie.Var.make(σ.x, σ.κ)
   } else
   if (Trie.Constr.is(σ)) {
      return Trie.Constr.make(σ.cases.map(
         ({ fst: ctr, snd: Π }: Pair<string, Args<K>>): Pair<string, Args<K>> => {
            return Pair.make(ctr, instantiateArgs(ρ, Π))
         })
      )
   } else {
      return absurd()
   }
}
