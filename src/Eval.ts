import { absurd } from "./util/Core"
import { PersistentObject, Versioned, make } from "./util/Persistent"
import { Cons, List, Nil } from "./BaseTypes"
import { Env, EmptyEnv, ExtendEnv } from "./Env"
import { Expr } from "./Expr"
import { instantiate } from "./Instantiate"
import { BinaryOp, binaryOps } from "./Primitive"
import { Traced, Value, Value̊ } from "./Traced"

import App = Traced.App
import BinaryApp = Traced.BinaryApp
import Empty = Traced.Empty
import Let = Traced.Let
import LetRec = Traced.LetRec
import MatchAs = Traced.MatchAs
import RecDef = Expr.RecDef
import Trie = Expr.Trie
import UnaryApp = Traced.UnaryApp
import Var = Traced.Var

type Tag = "val" | "trace"

// The "runtime identity" of an expression. In the formalism we use a "flat" representation so that e always has an external id;
// here it is more convenient to use an isomorphic nested format.
export class ExprId implements PersistentObject {
   j: List<Traced>
   e: Versioned<Expr | RecDef>

   constructor_ (j: List<Traced>, e: Versioned<Expr | RecDef>) {
      this.j = j
      this.e = e
   }

   static make<T extends Tag> (j: List<Traced>, e: Versioned<Expr | RecDef>): ExprId {
      return make(ExprId, j, e)
   }
}

export class Tagged<T extends Tag> implements PersistentObject {
   e: Expr
   tag: T

   constructor_ (e: Expr, tag: T) {
      this.e = e
      this.tag = tag
   }

   static make<T extends Tag> (e: Expr, tag: T): Tagged<T> {
      return make(Tagged, e, tag) as Tagged<T>
   }
}

export type ValId = Tagged<"val">
export type TraceId = Tagged<"trace">

export module Eval {

// Environments are snoc-lists, so this reverses declaration order, but semantically it's irrelevant.
export function closeDefs (δ_0: List<Expr.RecDef>, ρ: Env, δ: List<Expr.RecDef>): Env {
   if (Cons.is(δ)) {
      const e: Expr = δ.head.e,
            k: TraceId = Tagged.make(e, "trace"),
            kᵥ: ValId = Tagged.make(e, "val"),
            tv: Traced = Traced.make(Empty.at(k), Value.Closure.at(kᵥ, ρ, δ_0, Trie.Var.make(δ.head.x, e)))
      return ExtendEnv.make(closeDefs(δ_0, ρ, δ.tail), δ.head.x.str, tv)
   } else
   if (Nil.is(δ)) {
      return EmptyEnv.make()
   } else {
      return absurd()
   }
}

export function eval_ (ρ: Env, e: Expr): Traced {
   const k: TraceId = Tagged.make(e, "trace"),
         kᵥ: ValId = Tagged.make(e, "val")
   // An unevaluated expression has a bot trace for the sake of monotonicity across computations; might
   // want to reinstate the embedding of expressions into traces here.
   if (e instanceof Expr.Bot) {
     return Traced.make(Traced.Bot.at(k), null)
   } else
   if (e instanceof Expr.Constr) {
      return Traced.make(Empty.at(k), Value.Constr.at(kᵥ, e.ctr, e.args.map(e => eval_(ρ, e))))
   } else
   if (e instanceof Expr.ConstInt) {
      return Traced.make(Empty.at(k), Value.ConstInt.at(kᵥ, e.val))
   } else
   if (e instanceof Expr.ConstStr) {
      return Traced.make(Empty.at(k), Value.ConstStr.at(kᵥ, e.val))
   } else
   if (e instanceof Expr.Fun) {
      return Traced.make(Empty.at(k), Value.Closure.at(kᵥ, ρ, Nil.make(), e.σ))
   } else
   if (e instanceof Expr.PrimOp) {
      return Traced.make(Empty.at(k), Value.PrimOp.at(kᵥ, e.op))
   } else
   if (e instanceof Expr.Var) {
      const x: string = e.x.str
      if (ρ.has(x)) { 
         const {t, v}: Traced = ρ.get(x)!
         return Traced.make(Var.at(k, e.x, t), v)
      } else {
         return absurd("Variable not found.", x)
      }
   } else
   if (e instanceof Expr.App) {
      const tf: Traced = eval_(ρ, e.func),
            f: Value̊ = tf.v
      if (f instanceof Value.Closure) {
         const tu: Traced = eval_(ρ, e.arg),
               {ρ: ρʹ, κ: eʹ} = match(tu, f.σ),
               tv: Traced = eval_(Env.concat(f.ρ, ρʹ), instantiate(ρʹ, eʹ))
         return Traced.make(App.at(k, tf, tu, tv.t), tv.v)
      } else
      // Primitives with identifiers as names are unary and first-class.
      if (f instanceof Value.PrimOp) {
         const tu: Traced = eval_(ρ, e.arg)
         return Traced.make(UnaryApp.at(k, tf, tu), f.op.b.op(tu.v!)(kᵥ))
      } else {
         return absurd()
      }
   } else
   if (e instanceof Expr.Let) {
      const tu: Traced = eval_(ρ, e.e), 
            {ρ: ρʹ, κ: eʹ} = match(e.σ),
            tv: Traced = eval_(Env.concat(ρ, ρʹ), instantiate(ρʹ, eʹ))
      return Traced.make(Let.at(k, tu, Trie.Var.make(e.σ.x, tv.t)), tv.v)
   } else
   if (e instanceof Expr.LetRec) {
      const ρʹ: Env = closeDefs(e.δ, ρ, e.δ),
            tv: Traced = eval_(Env.concat(ρ, ρʹ), instantiate(ρʹ, e.e))
      return Traced.make(LetRec.at(k, e.δ, tv), tv.v)
   } else
   if (e instanceof Expr.MatchAs) {
      const tu: Traced = eval_(ρ, e.e),
            {ρ: ρʹ, κ: eʹ} = match(tu, e.σ),
            tv = eval_(Env.concat(ρ, ρʹ), instantiate(ρʹ, eʹ))
      return Traced.make(MatchAs.at(k, tu, e.σ, tv.t), tv.v)
   } else
   // Operators (currently all binary) are "syntax", rather than names.
   if (e instanceof Expr.BinaryApp) {
      if (binaryOps.has(e.opName.str)) {
         const op: BinaryOp = binaryOps.get(e.opName.str)!,
               tv1: Traced = eval_(ρ, e.e1),
               tv2: Traced = eval_(ρ, e.e2),
               v: Value = op.b.op(tv1.v!, tv2.v!)(kᵥ)
         return Traced.make(BinaryApp.at(k, tv1, e.opName, tv2), v)
      } else {
         return absurd("Operator name not found.", e.opName)
      }
   } else {
      return absurd("Unimplemented expression form.", e)
   }
}

}
