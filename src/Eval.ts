import { absurd, as, assert } from "./util/Core"
import { PersistentObject, Versioned, make, asVersioned } from "./util/Persistent"
import { ann, bot } from "./Annotated"
import { Cons, List, Nil } from "./BaseTypes"
import { Env, EmptyEnv, ExtendEnv } from "./Env"
import { ExplVal, Value } from "./ExplVal"
import { Expr } from "./Expr"
import { instantiate } from "./Instantiate"
import { lookup } from "./Match"
import { BinaryOp, binaryOps } from "./Primitive"

import App = ExplVal.App
import BinaryApp = ExplVal.BinaryApp
import Empty = ExplVal.Empty
import Fun = Expr.Fun
import Let = ExplVal.Let
import LetRec = ExplVal.LetRec
import MatchAs = ExplVal.MatchAs
import RecDef = Expr.RecDef
import Trie = Expr.Trie
import UnaryApp = ExplVal.UnaryApp
import Var = ExplVal.Var

type Tag = "val" | "expl"

// The "runtime identity" of an expression. In the formalism we use a "flat" representation so that e always has an external id;
// here it is more convenient to use an isomorphic nested format.
export class ExprId implements PersistentObject {
   j: List<ExplVal>
   e: Versioned<Expr | RecDef>

   constructor_ (j: List<ExplVal>, e: Versioned<Expr | RecDef>) {
      this.j = j
      this.e = e
   }

   static make<T extends Tag> (j: List<ExplVal>, e: Versioned<Expr | RecDef>): ExprId {
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
export type ExplId = Tagged<"expl">

export module Eval {

// Environments are snoc-lists, so this reverses declaration order, but semantically it's irrelevant.
export function closeDefs (δ_0: List<Expr.RecDef>, ρ: Env, δ: List<Expr.RecDef>): Env {
   if (Cons.is(δ)) {
      const f: Fun = δ.head.f,
            k: ExplId = Tagged.make(f, "expl"),
            kᵥ: ValId = Tagged.make(f, "val"),
            tv: ExplVal = ExplVal.make(ρ, Empty.at(k), Value.Closure.at(kᵥ, f.α, ρ, δ_0, f.σ))
      return ExtendEnv.make(closeDefs(δ_0, ρ, δ.tail), δ.head.x.str, tv)
   } else
   if (Nil.is(δ)) {
      return EmptyEnv.make()
   } else {
      return absurd()
   }
}

export function uncloseDefs (ρ: Env): [List<Expr.RecDef>, Env, List<Expr.RecDef>] {
   // ρ is a collection of one or more closures.
   const fs: List<Value.Closure> = ρ.entries().map(tv => as(tv.v, Value.Closure))
   assert (fs.length > 0)
   let ρʹ: Env | null = null
   fs.map((f: Value.Closure): null => {
      if (ρʹ !== null) {
         // ρʹ = join(ρʹ, f.ρ)
      }
      return null
   })
   return absurd()
}

export function eval_ (ρ: Env, e: Expr): ExplVal {
   const k: ExplId = Tagged.make(e, "expl"),
         kᵥ: ValId = Tagged.make(e, "val")
   if (e instanceof Expr.ConstInt) {
      return ExplVal.make(ρ, Empty.at(k), Value.ConstInt.at(kᵥ, e.α, e.val))
   } else
   if (e instanceof Expr.ConstStr) {
      return ExplVal.make(ρ, Empty.at(k), Value.ConstStr.at(kᵥ, e.α, e.val))
   } else
   if (e instanceof Expr.Fun) {
      return ExplVal.make(ρ, Empty.at(k), Value.Closure.at(kᵥ, e.α, ρ, Nil.make(), e.σ))
   } else
   if (e instanceof Expr.PrimOp) {
      return ExplVal.make(ρ, Empty.at(k), Value.PrimOp.at(kᵥ, e.α, e.op))
   } else
   if (e instanceof Expr.Constr) {
      return ExplVal.make(ρ, Empty.at(k), Value.Constr.at(kᵥ, e.α, e.ctr, e.args.map(e => eval_(ρ, e))))
   } else
   if (e instanceof Expr.Var) {
      const x: string = e.x.str
      if (ρ.has(x)) { 
         const {t, v}: ExplVal = ρ.get(x)!
         return ExplVal.make(ρ, Var.at(k, e.x, t), v.copyAt(kᵥ, ann.meet(v.α, e.α)))
      } else {
         return absurd("Variable not found.", x)
      }
   } else
   if (e instanceof Expr.App) {
      const tf: ExplVal = eval_(ρ, e.func),
            f: Value = tf.v
      if (f instanceof Value.Closure) {
         const tu: ExplVal = eval_(ρ, e.arg),
               [ρʹ, eʹ, α] = lookup(tu, f.σ),
               ρᶠ: Env = Env.concat(f.ρ, closeDefs(f.δ, f.ρ, f.δ)),
               tv: ExplVal = eval_(Env.concat(ρᶠ, ρʹ), instantiate(ρʹ, eʹ))
         return ExplVal.make(ρ, App.at(k, tf, tu, tv), tv.v.copyAt(kᵥ, ann.meet(f.α, α, tv.v.α, e.α)))
      } else
      // Primitives with identifiers as names are unary and first-class.
      if (f instanceof Value.PrimOp) {
         const tu: ExplVal = eval_(ρ, e.arg)
         return ExplVal.make(ρ, UnaryApp.at(k, tf, tu), f.op.b.op(tu.v!)(kᵥ, ann.meet(f.α, tu.v.α, e.α)))
      } else {
         return absurd()
      }
   } else
   // Operators (currently all binary) are "syntax", rather than names.
   if (e instanceof Expr.BinaryApp) {
      if (binaryOps.has(e.opName.str)) {
         const op: BinaryOp = binaryOps.get(e.opName.str)!, // opName lacks annotations
               [tv1, tv2]: [ExplVal, ExplVal] = [eval_(ρ, e.e1), eval_(ρ, e.e2)],
               v: Value = op.b.op(tv1.v!, tv2.v!)(kᵥ, ann.meet(tv1.v.α, tv2.v.α, e.α))
         return ExplVal.make(ρ, BinaryApp.at(k, tv1, e.opName, tv2), v)
      } else {
         return absurd("Operator name not found.", e.opName)
      }
   } else
   if (e instanceof Expr.Let) {
      const tu: ExplVal = eval_(ρ, e.e),
            [ρʹ, eʹ, α] = lookup(tu, e.σ),
            {t, v}: ExplVal = eval_(Env.concat(ρ, ρʹ), instantiate(ρʹ, eʹ))
      return ExplVal.make(ρ, Let.at(k, tu, Trie.Var.make(e.σ.x, t)), v.copyAt(kᵥ, ann.meet(α, v.α, e.α)))
   } else
   if (e instanceof Expr.LetRec) {
      const ρʹ: Env = closeDefs(e.δ, ρ, e.δ),
            tv: ExplVal = eval_(Env.concat(ρ, ρʹ), instantiate(ρʹ, e.e))
      return ExplVal.make(ρ, LetRec.at(k, e.δ, tv), tv.v.copyAt(kᵥ, ann.meet(tv.v.α, e.α)))
   } else
   if (e instanceof Expr.MatchAs) {
      const tu: ExplVal = eval_(ρ, e.e),
            [ρʹ, eʹ, α] = lookup(tu, e.σ),
            {t, v} = eval_(Env.concat(ρ, ρʹ), instantiate(ρʹ, eʹ))
      return ExplVal.make(ρ, MatchAs.at(k, tu, e.σ, t), v.copyAt(kᵥ, ann.meet(α, v.α, e.α)))
   } else {
      return absurd("Unimplemented expression form.", e)
   }
}

// Output environment is written to.
export function uneval ({ρ, t, v}: ExplVal): Expr {
   const kᵥ: ValId = asVersioned(v).__id as ValId,
         k: ExprId = asVersioned(kᵥ.e).__id as ExprId
   if (t instanceof Empty) {
      if (v instanceof Value.ConstInt) {
         bot(ρ)
         return Expr.ConstInt.at(k, v.α, v.val)
      } else
      if (v instanceof Value.ConstStr) {
         bot(ρ)
         return Expr.ConstStr.at(k, v.α, v.val)
      } else
      if (v instanceof Value.Closure) {
         assert(v.δ.length === 0)
         bot(ρ)
         return Expr.Fun.at(k, v.α, v.σ)
      } else 
      if (v instanceof Value.PrimOp) {
         bot(ρ)
         return Expr.PrimOp.at(k, v.α, v.op)
      } else
      if (v instanceof Value.Constr) {
         // reverse order but shouldn't matter in absence of side-effects:
         return Expr.Constr.at(k, v.α, v.ctr, v.args.map(uneval))
      } else {
         return absurd()
      }
   } else
   if (t instanceof Var) {
      const x: string = t.x.str
      bot(ρ)
      assert(ρ.has(x))
         ρ.get(x)!.v.setα(v.α)
         return Expr.Var.at(k, v.α, t.x)
   }
   else
   if (t instanceof App) {
      const f: Value.Closure | Value.PrimOp = t.func.v as (Value.Closure | Value.PrimOp)
      if (f instanceof Value.Closure) {
         return Expr.App.at(k, v.α, uneval(t.func).setα(v.α), uneval(t.arg).setα(v.α))
      } else
      if (f instanceof Value.PrimOp) {
         return Expr.App.at(k, v.α, uneval(t.func).setα(v.α), uneval(t.arg).setα(v.α))
      } else {
         return absurd()
      }
   } else
   if (t instanceof BinaryApp) {
      assert(binaryOps.has(t.opName.str))
      return Expr.BinaryApp.at(k, v.α, uneval(t.tv1).setα(v.α), t.opName, uneval(t.tv2).setα(v.α))
   } else {
      return absurd()
   }
}

}
