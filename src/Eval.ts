import { absurd } from "./util/Core"
import { PersistentObject, Versioned, make } from "./util/Persistent"
import { ann } from "./Annotated"
import { Cons, List, Nil } from "./BaseTypes"
import { Env, EmptyEnv, ExtendEnv } from "./Env"
import { Expr } from "./Expr"
import { instantiate } from "./Instantiate"
import { lookup } from "./Match"
import { BinaryOp, binaryOps } from "./Primitive"
import { Traced, Value } from "./Traced"

import App = Traced.App
import BinaryApp = Traced.BinaryApp
import Empty = Traced.Empty
import Fun = Expr.Fun
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
      const f: Fun = δ.head.f,
            k: TraceId = Tagged.make(f, "trace"),
            kᵥ: ValId = Tagged.make(f, "val"),
            tv: Traced = Traced.make(Empty.at(k), Value.Closure.at(kᵥ, f.α, ρ, δ_0, f.σ))
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
   if (e instanceof Expr.Constr) {
      return Traced.make(Empty.at(k), Value.Constr.at(kᵥ, e.α, e.ctr, e.args.map(e => eval_(ρ, e))))
   } else
   if (e instanceof Expr.ConstInt) {
      return Traced.make(Empty.at(k), Value.ConstInt.at(kᵥ, e.α, e.val))
   } else
   if (e instanceof Expr.ConstStr) {
      return Traced.make(Empty.at(k), Value.ConstStr.at(kᵥ, e.α, e.val))
   } else
   if (e instanceof Expr.Fun) {
      return Traced.make(Empty.at(k), Value.Closure.at(kᵥ, e.α, ρ, Nil.make(), e.σ))
   } else
   if (e instanceof Expr.PrimOp) {
      return Traced.make(Empty.at(k), Value.PrimOp.at(kᵥ, e.α, e.op))
   } else
   if (e instanceof Expr.Var) {
      const x: string = e.x.str
      if (ρ.has(x)) { 
         const {t, v}: Traced = ρ.get(x)!
         return Traced.make(Var.at(k, e.x, t), v.copyAt(kᵥ, ann.meet(v.α, e.α)))
      } else {
         return absurd("Variable not found.", x)
      }
   } else
   if (e instanceof Expr.App) {
      const tf: Traced = eval_(ρ, e.func),
            f: Value = tf.v
      if (f instanceof Value.Closure) {
         const tu: Traced = eval_(ρ, e.arg),
               [ρʹ, eʹ] = lookup(tu, f.σ),
               ρᶠ: Env = Env.concat(f.ρ, closeDefs(f.δ, f.ρ, f.δ)),
               {t, v}: Traced = eval_(Env.concat(ρᶠ, ρʹ), instantiate(ρʹ, eʹ))
         return Traced.make(App.at(k, tf, tu, t), v.copyAt(kᵥ, ann.meet(f.α, v.α, e.α)))
      } else
      // Primitives with identifiers as names are unary and first-class.
      if (f instanceof Value.PrimOp) {
         const tu: Traced = eval_(ρ, e.arg)
         return Traced.make(UnaryApp.at(k, tf, tu), f.op.b.op(tu.v!)(kᵥ, ann.meet(f.α, tu.v.α, e.α)))
      } else {
         return absurd()
      }
   } else
   // Operators (currently all binary) are "syntax", rather than names.
   if (e instanceof Expr.BinaryApp) {
      if (binaryOps.has(e.opName.str)) {
         const op: BinaryOp = binaryOps.get(e.opName.str)!, // opName lacks annotations
               [tv1, tv2]: [Traced, Traced] = [eval_(ρ, e.e1), eval_(ρ, e.e2)],
               v: Value = op.b.op(tv1.v!, tv2.v!)(kᵥ, ann.meet(e.e1.α, e.e2.α, e.α))
         return Traced.make(BinaryApp.at(k, tv1, e.opName, tv2), v)
      } else {
         return absurd("Operator name not found.", e.opName)
      }
   } else
   if (e instanceof Expr.Let) {
      const tu: Traced = eval_(ρ, e.e), 
            [ρʹ, eʹ] = lookup(tu, e.σ),
            {t, v}: Traced = eval_(Env.concat(ρ, ρʹ), instantiate(ρʹ, eʹ))
      return Traced.make(Let.at(k, tu, Trie.Var.make(e.σ.x, t)), v.copyAt(kᵥ, ann.bot))
   } else
   if (e instanceof Expr.LetRec) {
      const ρʹ: Env = closeDefs(e.δ, ρ, e.δ),
            tv: Traced = eval_(Env.concat(ρ, ρʹ), instantiate(ρʹ, e.e))
      return Traced.make(LetRec.at(k, e.δ, tv), tv.v.copyAt(kᵥ, ann.bot))
   } else
   if (e instanceof Expr.MatchAs) {
      const tu: Traced = eval_(ρ, e.e),
            [ρʹ, eʹ] = lookup(tu, e.σ),
            {t, v} = eval_(Env.concat(ρ, ρʹ), instantiate(ρʹ, eʹ))
      return Traced.make(MatchAs.at(k, tu, e.σ, t), v.copyAt(kᵥ, ann.bot))
   } else {
      return absurd("Unimplemented expression form.", e)
   }
}

}
