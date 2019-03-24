import { ann } from "./util/Annotated"
import { absurd, as, assert } from "./util/Core"
import { PersistentObject, make } from "./util/Persistent"
import { asVersioned } from "./util/Versioned"
import { Cons, List, Nil, nil } from "./BaseTypes"
import { Env, ExtendEnv } from "./Env"
import { ExplVal, Match, Value, explVal } from "./ExplVal"
import { Expr } from "./Expr"
import { instantiate, uninstantiate } from "./Instantiate"
import { match, matchVar, unmatch } from "./Match"
import { BinaryOp, binaryOps } from "./Primitive"

import App = ExplVal.App
import BinaryApp = ExplVal.BinaryApp
import Empty = ExplVal.Empty
import Let = ExplVal.Let
import LetRec = ExplVal.LetRec
import MatchAs = ExplVal.MatchAs
import UnaryApp = ExplVal.UnaryApp
import Var = ExplVal.Var

import app = ExplVal.app
import binaryApp = ExplVal.binaryApp
import empty = ExplVal.empty
import let_ = ExplVal.let_
import letRec = ExplVal.letRec
import matchAs = ExplVal.matchAs
import RecDef = Expr.RecDef
import unaryApp = ExplVal.unaryApp
import var_ = ExplVal.var_

type Tag = "t" | "v" // TODO: expess in terms of keyof ExplVal?

export class Tagged<T extends Tag> implements PersistentObject {
   e: Expr | RecDef
   tag: T

   constructor_ (e: Expr | RecDef, tag: T) {
      this.e = e
      this.tag = tag
   }
}

export function tagged<T extends Tag> (e: Expr | RecDef, tag: T): Tagged<T> {
   return make(Tagged, e, tag) as Tagged<T>
}

// User-level error.
export function error (msg: string, ...x̅: any[]): any {
   if (x̅.length > 0) {
      console.warn("Error data:\n")
      x̅.forEach(x => console.warn(x))
   }
   throw new Error("User error")
}

export type ValId = Tagged<"v">
export type ExplId = Tagged<"t">

export module Eval {

// Environments are snoc-lists, so this reverses declaration order, but semantically it's irrelevant.
export function closeDefs (δ_0: List<Expr.RecDef>, ρ: Env, δ: List<Expr.RecDef>): Env {
   if (Cons.is(δ)) {
      const def: RecDef = δ.head,
            kᵥ: ValId = tagged(def, "v")
      return ExtendEnv.make(closeDefs(δ_0, ρ, δ.tail), def.x.str, Value.closure(kᵥ, def.α, ρ, δ_0, def.σ))
   } else
   if (Nil.is(δ)) {
      return Env.empty()
   } else {
      return absurd()
   }
}

// ρ is a collection of one or more closures. Most of the required joins have already been computed.
export function uncloseDefs (ρ: Env): [Env, List<Expr.RecDef>] {
   const f̅: List<Value.Closure> = ρ.entries().map((v: Value) => as(v, Value.Closure))
   if (Cons.is(f̅)) {
      let δ: List<RecDef> = f̅.head.δ,
          f̅ʹ: List<Value.Closure> = f̅
      for (; Cons.is(f̅ʹ) && Cons.is(δ); f̅ʹ = f̅ʹ.tail, δ = δ.tail) {
         δ.head.joinα(f̅ʹ.head.α)
      }
      return [f̅.head.ρ, f̅.head.δ]
   } else
   if (Nil.is(f̅)) {
      return [Env.empty(), nil()]
   } else {
      return absurd()
   }
}

export function eval_ (ρ: Env, e: Expr): ExplVal {
   const k: ExplId = tagged(e, "t"),
         kᵥ: ValId = tagged(e, "v")
   if (e instanceof Expr.ConstInt) {
      return explVal(ρ, empty(k), Value.constInt(kᵥ, e.α, e.val))
   } else
   if (e instanceof Expr.ConstStr) {
      return explVal(ρ, empty(k), Value.constStr(kᵥ, e.α, e.val))
   } else
   if (e instanceof Expr.Fun) {
      return explVal(ρ, empty(k), Value.closure(kᵥ, e.α, ρ, nil(), e.σ))
   } else
   if (e instanceof Expr.PrimOp) {
      return explVal(ρ, empty(k), Value.primOp(kᵥ, e.α, e.op))
   } else
   if (e instanceof Expr.Constr) {
      return explVal(ρ, empty(k), Value.constr(kᵥ, e.α, e.ctr, e.args.map(e => eval_(ρ, e))))
   } else
   if (e instanceof Expr.Var) {
      const x: string = e.x.str
      if (ρ.has(x)) { 
         const v: Value = ρ.get(x)!
         return explVal(ρ, var_(k, e.x), v.copyAt(kᵥ, ann.meet(v.α, e.α)))
      } else {
         return error("Variable not found.", x)
      }
   } else
   if (e instanceof Expr.App) {
      const tf: ExplVal = eval_(ρ, e.func),
            f: Value = tf.v
      if (f instanceof Value.Closure) {
         const tu: ExplVal = eval_(ρ, e.arg),
               [{ξ, κ: eʹ}, α] = match(tu.v, f.σ),
               ρ_defs: Env = closeDefs(f.δ, f.ρ, f.δ),
               ρʹ: Env = Env.concat(ρ_defs, ξ.ρ),
               tv: ExplVal = eval_(Env.concat(f.ρ, ρʹ), instantiate(ρʹ, eʹ))
         return explVal(ρ, app(k, tf, tu, ρ_defs, Match.plug(ξ, tv)), tv.v.copyAt(kᵥ, ann.meet(f.α, α, tv.v.α, e.α)))
      } else
      // Primitives with identifiers as names are unary and first-class.
      if (f instanceof Value.PrimOp) {
         const tu: ExplVal = eval_(ρ, e.arg)
         return explVal(ρ, unaryApp(k, tf, tu), f.op.b.op(tu.v!)(kᵥ, ann.meet(f.α, tu.v.α, e.α)))
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
         return explVal(ρ, binaryApp(k, tv1, e.opName, tv2), v)
      } else {
         return error("Operator name not found.", e.opName)
      }
   } else
   if (e instanceof Expr.Let) {
      const tu: ExplVal = eval_(ρ, e.e),
            [{ξ, κ: eʹ}, α] = matchVar(tu.v, e.σ),
            tv: ExplVal = eval_(Env.concat(ρ, ξ.ρ), instantiate(ξ.ρ, eʹ))
      return explVal(ρ, let_(k, tu, Match.plug(ξ, tv)), tv.v.copyAt(kᵥ, ann.meet(α, tv.v.α, e.α)))
   } else
   if (e instanceof Expr.LetRec) {
      const ρʹ: Env = closeDefs(e.δ, ρ, e.δ),
            tv: ExplVal = eval_(Env.concat(ρ, ρʹ), instantiate(ρʹ, e.e))
      return explVal(ρ, letRec(k, e.δ, ρʹ, tv), tv.v.copyAt(kᵥ, ann.meet(tv.v.α, e.α)))
   } else
   if (e instanceof Expr.MatchAs) {
      const tu: ExplVal = eval_(ρ, e.e),
            [{ξ, κ: eʹ}, α] = match(tu.v, e.σ),
            tv: ExplVal = eval_(Env.concat(ρ, ξ.ρ), instantiate(ξ.ρ, eʹ))
      return explVal(ρ, matchAs(k, tu, Match.plug(ξ, tv)), tv.v.copyAt(kᵥ, ann.meet(α, tv.v.α, e.α)))
   } else {
      return absurd("Unimplemented expression form.", e)
   }
}

// Output environment is written to.
export function uneval ({ρ, t, v}: ExplVal): Expr {
   const k: ExplId = asVersioned(t).__id as ExplId,
         e: Expr = k.e as Expr
   if (t instanceof Empty) {
      if (v instanceof Value.ConstInt) {
         return e.joinα(v.α)
      } else
      if (v instanceof Value.ConstStr) {
         return e.joinα(v.α)
      } else
      if (v instanceof Value.Closure) {
         assert(v.δ.length === 0)
         return e.joinα(v.α)
      } else 
      if (v instanceof Value.PrimOp) {
         return e.joinα(v.α)
      } else
      if (v instanceof Value.Constr) {
         // reverse order but shouldn't matter in absence of side-effects:
         v.args.map(uneval)
         return e.joinα(v.α)
      } else {
         return absurd()
      }
   } else
   if (t instanceof Var) {
      const x: string = t.x.str
      assert(ρ.has(x))
      ρ.get(x)!.joinα(v.α)
      return e.joinα(v.α)
   }
   else
   if (t instanceof App) {
      assert(t.func.v instanceof Value.Closure)
      const {ξ, κ: tv} = t.ξtv
      tv.v.joinα(v.α)
      unmatch(Match.plug(ξ, uninstantiate(uneval(tv))), v.α)
      uncloseDefs(t.ρ_defs)
      t.func.v.joinα(v.α)
      uneval(t.func)
      uneval(t.arg)
      return e.joinα(v.α)
   } else
   if (t instanceof UnaryApp) {
      assert(t.func.v instanceof Value.PrimOp)
      t.func.v.joinα(v.α)
      t.arg.v.joinα(v.α)
      uneval(t.func)
      uneval(t.arg)
      return e.joinα(v.α)
   } else
   if (t instanceof BinaryApp) {
      assert(binaryOps.has(t.opName.str))
      t.tv1.v.joinα(v.α)
      t.tv2.v.joinα(v.α)
      uneval(t.tv1)
      uneval(t.tv2)
      return e.joinα(v.α)
   } else
   if (t instanceof Let) {
      const {κ: tv} = t.ξtv
      tv.v.joinα(v.α)
      uninstantiate(uneval(tv))
      uneval(t.tu) // unmatch not required - suffices to uneval in reverse order
      return e.joinα(v.α)
   } else
   if (t instanceof LetRec) {
      t.tv.v.joinα(v.α)
      uninstantiate(uneval(t.tv))
      uncloseDefs(t.ρ_defs)
      return e.joinα(v.α)
   } else
   if (t instanceof MatchAs) {
      const {ξ, κ: tv} = t.ξtv
      tv.v.joinα(v.α)
      unmatch(Match.plug(ξ, uninstantiate(uneval(tv))), v.α)
      uneval(t.tu)
      return e.joinα(v.α)
   } else {
      return absurd()
   }
}

}
