import { __nonNull, absurd, className, error } from "./util/Core"
import { Cons, List, Nil, pair } from "./BaseTypes2"
import { ctrFor } from "./DataType2"
import { Expl, ExplValue, explValue } from "./ExplValue2"
import { Expr } from "./Expr2"
import { Env, Func, concat, emptyEnv, extendEnv, get, has } from "./Func2"
import { evalTrie } from "./Match2"
import { BinaryOp, binaryOps } from "./Primitive2"
import { Value, _, make } from "./Value2"

import Trie = Expr.Trie

export module Eval {

// Environments are snoc-lists, so this (inconsequentially) reverses declaration order.
export function closeDefs (δ_0: List<Expr.RecDef>, ρ: Env, δ: List<Expr.RecDef>): Env {
   if (Cons.is(δ)) {
      const { σ, x }: Expr.RecDef = δ.head
      return extendEnv(closeDefs(δ_0, ρ, δ.tail), x.val, recFunc(σ, ρ, δ_0))
   } else
   if (Nil.is(δ)) {
      return emptyEnv()
   } else {
      return absurd()
   }
}

// I still have expressions in the "semantic" domain, because we have to construct the environment
// as we go along for recursion.
class RecFunc extends Func {
   σ: Trie<Expr> = _
   ρ: Env = _
   δ: List<Expr.RecDef> = _

   __apply (v: Value): Value {
      return evalTrie(concat(this.ρ, closeDefs(this.δ, this.ρ, this.δ)), this.σ).__apply(v)
   }
}

function recFunc (σ: Trie<Expr>, ρ: Env, δ: List<Expr.RecDef>): RecFunc {
   return make(RecFunc, σ, ρ, δ)
}

export function eval_ (ρ: Env, e: Expr): ExplValue {
   if (e instanceof Expr.ConstNum) {
      return explValue(Expl.empty(), e.val)
   } else
   if (e instanceof Expr.ConstStr) {
      return explValue(Expl.empty(), e.val)
   } else
   if (e instanceof Expr.Fun) {
      return explValue(Expl.empty(), evalTrie(ρ, e.σ))
   } else
   if (e instanceof Expr.PrimOp) {
      return explValue(Expl.empty(), e.op)
   } else
   if (e instanceof Expr.Constr) {
      let v̅: Value[] = e.args.toArray().map((e: Expr) => eval_(ρ, e).v)
      return explValue(Expl.empty(), make(ctrFor(e.ctr).C, ...v̅))
   } else 
   if (e instanceof Expr.Var) {
      if (has(ρ, e.x.val)) { 
         return explValue(Expl.var_(e.x), get(ρ, e.x.val)!)
      } else {
         return error(`Variable '${e.x.val}' not found.`)
      }
   } else
   if (e instanceof Expr.App) {
      const tf: ExplValue = eval_(ρ, e.func)
      if (tf.v instanceof Func) {
         const tu: ExplValue = eval_(ρ, e.arg),
               tv: ExplValue = explValue(Expl.empty(), tf.v.__apply(tu.v)) // for now
         return explValue(Expl.app(tf, tu, tv), tv.v)
      } else {
         return error(`Cannot apply a ${className(tf.v)}`)
      }
   } else
   // Operators (currently all binary) are "syntax", rather than names.
   if (e instanceof Expr.BinaryApp) {
      if (binaryOps.has(e.opName.str)) {
         const op: BinaryOp = binaryOps.get(e.opName.str)!, // opName lacks annotations
               [tv1, tv2]: [ExplValue, ExplValue] = [eval_(ρ, e.e1), eval_(ρ, e.e2)]
         return explValue(Expl.binaryApp(tv1, e.opName, tv2), op.__apply(pair(tv1.v, tv2.v)))
      } else {
         return error(`Operator ${e.opName.str} not found.`)
      }
   } else
   if (e instanceof Expr.Let) {
      const tu: ExplValue = eval_(ρ, e.e),
            tv: ExplValue = explValue(Expl.empty(), evalTrie(ρ, e.σ).__apply(tu.v)) // for now
      return explValue(Expl.let_(tu, tv), tv.v)
   } else
   if (e instanceof Expr.LetRec) {
      const ρʹ: Env = closeDefs(e.δ, ρ, e.δ),
            tv: ExplValue = eval_(concat(ρ, ρʹ), e.e)
      return explValue(Expl.letRec(e.δ, ρʹ, tv), tv.v)
   } else
   if (e instanceof Expr.MatchAs) {
      const tu: ExplValue = eval_(ρ, e.e),
            tv: ExplValue = explValue(Expl.empty(), evalTrie(ρ, e.σ).__apply(tu.v))
      return explValue(Expl.matchAs(tu, tv), tv.v)
   } else {
      return absurd(`Unimplemented expression form: ${className(e)}.`)
   }
}

}
