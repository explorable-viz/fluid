import { absurd } from "./util/Core"
import { PersistentObject, Versioned, asVersioned } from "./util/Persistent"
import { Annotation, ann } from "./Annotated"
import { List, Pair } from "./BaseTypes"
import { Env } from "./Env"
import { ExprId, exprId } from "./Eval"
import { Expr, Kont } from "./Expr"

import App = Expr.App
import Args = Expr.Args
import BinaryApp = Expr.BinaryApp
import ConstInt = Expr.ConstInt
import ConstStr = Expr.ConstStr
import Constr = Expr.Constr
import Fun = Expr.Fun
import Let = Expr.Let
import LetRec = Expr.LetRec
import MatchAs = Expr.MatchAs
import PrimOp = Expr.PrimOp
import RecDef = Expr.RecDef
import Trie = Expr.Trie
import Var = Expr.Var

export function instantiate (ρ: Env, e: Expr): Expr {
   const j: ExprId = exprId(ρ.entries(), asVersioned(e))
   if (e instanceof ConstInt) {
      return ConstInt.at(j, e.α, e.val)
   } else
   if (e instanceof ConstStr) {
      return ConstStr.at(j, e.α, e.val)
   } else
   if (e instanceof Constr) {
      return Constr.at(j, e.α, e.ctr, e.args.map(e => instantiate(ρ, e)))
   } else
   if (e instanceof Fun) {
      return Fun.at(j, e.α, instantiateTrie(ρ, e.σ))
   } else
   if (e instanceof PrimOp) {
      return PrimOp.at(j, e.α, e.op)
   } else
   if (e instanceof Var) {
      return Var.at(j, e.α, e.x)
   } else
   if (e instanceof Let) {
      return Let.at(j, e.α, instantiate(ρ, e.e), instantiateTrie(ρ, e.σ))
   } else
   if (e instanceof LetRec) {
      const δ: List<RecDef> = e.δ.map(def => {
         const i: ExprId = exprId(ρ.entries(), asVersioned(def))
         return RecDef.at(i, def.x, instantiate(ρ, def.f) as Fun)
      })
      return LetRec.at(j, e.α, δ, instantiate(ρ, e.e))
   } else
   if (e instanceof MatchAs) {
      return MatchAs.at(j, e.α, instantiate(ρ, e.e), instantiateTrie(ρ, e.σ))
   } else
   if (e instanceof App) {
      return App.at(j, e.α, instantiate(ρ, e.func), instantiate(ρ, e.arg))
   } else
   if (e instanceof BinaryApp) {
      return BinaryApp.at(j, e.α, instantiate(ρ, e.e1), e.opName, instantiate(ρ, e.e2))
   } else {
      return absurd()
   }
}

// It's enough just to return original expression; reconstructing environment would require rethinking. 
export function uninstantiate (e: Expr): Expr {
   const eʹ: Versioned<Expr> = (asVersioned(e).__id as ExprId).e as Versioned<Expr>,
         k: PersistentObject = eʹ.__id,
         α: Annotation = ann.join(eʹ.α, e.α) // uninstantiate must merge annotations into the source
   if (e instanceof ConstInt) {
      return ConstInt.at(k, α, e.val)
   } else
   if (e instanceof ConstStr) {
      return ConstStr.at(k, α, e.val)
   } else
   if (e instanceof Constr) {
      return Constr.at(k, α, e.ctr, e.args.map(e => uninstantiate(e)))
   } else
   if (e instanceof Fun) {
      return Fun.at(k, α, uninstantiateTrie(e.σ))
   } else
   if (e instanceof PrimOp) {
      return PrimOp.at(k, α, e.op)
   } else
   if (e instanceof Var) {
      return Var.at(k, α, e.x)
   } else
   if (e instanceof Let) {
      return Let.at(k, α, uninstantiate(e.e), uninstantiateTrie(e.σ))
   } else {
      return absurd()
   }
}

// F-bounded polymorphism doesn't really work well here.
function instantiateTrie<K extends Kont<K>, T extends Trie<K>> (ρ: Env, σ: T): T {
   if (Trie.Var.is(σ)) {
      return Trie.Var.make(σ.x, instantiateKont(ρ, σ.κ) as K) as Trie<K> as T
   } else
   if (Trie.Constr.is(σ)) {
      return Trie.Constr.make(σ.cases.map(
         ({ fst: ctr, snd: Π }: Pair<string, Args<K>>): Pair<string, Args<K>> => {
            return Pair.make(ctr, instantiateArgs(ρ, Π))
         })
      ) as Trie<K> as T
   } else {
      return absurd()
   }
}

function uninstantiateTrie<K extends Kont<K>, T extends Trie<K>> (σ: T): T {
   if (Trie.Var.is(σ)) {
      return Trie.Var.make(σ.x, uninstantiateKont(σ.κ)) as Trie<K> as T
   } else {
      return absurd()
   }
}

// See issue #33. These is some sort of heinousness to covert the continuation type.
function instantiateKont<K extends Kont<K>> (ρ: Env, κ: K): K {
   if (κ instanceof Trie.Trie) {
      return instantiateTrie<K>(ρ, κ) as K // ouch
   } else
   if (κ instanceof Expr.Expr) {
      return instantiate(ρ, κ) as any as K // ouch
   } else
   if (κ instanceof Args.Args) {
      return instantiateArgs(ρ, κ) as K    // also ouch
   } else {
      return absurd()
   }
}

function uninstantiateKont<K extends Kont<K>> (κ: K): K {
   if (κ instanceof Trie.Trie) {
      return uninstantiateTrie<K>(κ) as K
   } else
   if (κ instanceof Expr.Expr) {
      return uninstantiate(κ) as any as K
   } else
   if (κ instanceof Args.Args) {
      return uninstantiateArgs(κ) as K
   } else {
      return absurd()
   }
}

function instantiateArgs<K extends Kont<K>> (ρ: Env, Π: Args<K>): Args<K> {
   if (Args.End.is(Π)) {
      return Args.End.make(instantiateKont(ρ, Π.κ))
   } else
   if (Args.Next.is(Π)) {
      return Args.Next.make(instantiateTrie(ρ, Π.σ))
   } else {
      return absurd()
   }
}

function uninstantiateArgs<K extends Kont<K>> (Π: Args<K>): Args<K> {
   if (Args.End.is(Π)) {
      return Args.End.make(uninstantiateKont(Π.κ))
   } else
   if (Args.Next.is(Π)) {
      return Args.Next.make(uninstantiateTrie(Π.σ))
   } else {
      return absurd()
   }
}
