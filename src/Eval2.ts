import { __nonNull, absurd, className, error } from "./util/Core"
import { Cons, List, Nil, pair } from "./BaseTypes2"
import { ctrFor } from "./DataType2"
import { Expr } from "./Expr2"
import { Env, Func, emptyEnv, extendEnv } from "./Func2"
import { evalTrie } from "./Match2"
import { BinaryOp, binaryOps } from "./Primitive2"
import { Value, _, make, num, str } from "./Value2"

import Trie = Expr.Trie

export module Eval {

// Environments are snoc-lists, so this (inconsequentially) reverses declaration order.
export function closeDefs (δ_0: List<Expr.RecDef>, ρ: Env, δ: List<Expr.RecDef>): Env {
   if (Cons.is(δ)) {
      const { σ, x }: Expr.RecDef = δ.head
      return extendEnv(closeDefs(δ_0, ρ, δ.tail), x.str, recFunc(σ, ρ, δ_0))
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
      return evalTrie(Env.concat(this.ρ, closeDefs(this.δ, this.ρ, this.δ)), this.σ).__apply(v)
   }
}

function recFunc (σ: Trie<Expr>, ρ: Env, δ: List<Expr.RecDef>): RecFunc {
   return make(RecFunc, σ, ρ, δ)
}

export function eval_ (ρ: Env, e: Expr): Value {
   if (e instanceof Expr.ConstNum) {
      return num(e.val)
   } else
   if (e instanceof Expr.ConstStr) {
      return str(e.val)
   } else
   if (e instanceof Expr.Fun) {
      return evalTrie(ρ, e.σ)
   } else
   if (e instanceof Expr.Var) {
      const x: string = e.x
      if (ρ.has(x)) { 
         return ρ.get(x)!
      } else {
         return error(`Variable '${x}' not found.`)
      }
   } else
   if (e instanceof Expr.PrimOp) {
      return e.op
   } else
   if (e instanceof Expr.App) {
      const v: Value = eval_(ρ, e.func)
      if (v instanceof Func) {
         return v.__apply(eval_(ρ, e.arg))
      } else {
         return error(`Cannot apply a ${className(v)}`, v)
      }
   } else
   // Operators (currently all binary) are "syntax", rather than names.
   if (e instanceof Expr.BinaryApp) {
      if (binaryOps.has(e.opName.str)) {
         const op: BinaryOp = binaryOps.get(e.opName.str)!, // opName lacks annotations
               [v1, v2]: [Value, Value] = [eval_(ρ, e.e1), eval_(ρ, e.e2)]
         return op.__apply(pair(v1, v2))
      } else {
         return error(`Operator ${e.opName} not found.`)
      }
   } else
   if (e instanceof Expr.Constr) {
      let v̅: Value[] = e.args.toArray().map((e: Expr) => eval_(ρ, e))
      return make(ctrFor(e.ctr).C, ...v̅)
   } else 
   if (e instanceof Expr.Let) {
      return evalTrie(ρ, e.σ).__apply(eval_(ρ, e.e))
   } else
   if (e instanceof Expr.LetRec) {
      const ρʹ: Env = closeDefs(e.δ, ρ, e.δ)
      return eval_(Env.concat(ρ, ρʹ), e.e)
   } else
   if (e instanceof Expr.MatchAs) {
      return evalTrie(ρ, e.σ).__apply(eval_(ρ, e.e))
   } else {
      return absurd(`Unimplemented expression form: ${className(e)}.`)
   }
}

}
