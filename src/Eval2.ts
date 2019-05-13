import { __nonNull, absurd, className, error } from "./util/Core"
import { Cons, List, Nil, pair } from "./BaseTypes2"
import { ctrFor } from "./DataType2"
import { Expr } from "./Expr2"
import { Env, Func, emptyEnv, extendEnv } from "./Func2"
import { eval_Trie } from "./Match2"
import { BinaryOp, binaryOps } from "./Primitive2"
import { Value, make, num, str } from "./Value2"

export module Eval {

// Environments are snoc-lists, so this reverses declaration order, but semantically it's irrelevant.
export function closeDefs (δ_0: List<Expr.RecDef>, ρ: Env, δ: List<Expr.RecDef>): Env {
   if (Cons.is(δ)) {
      const def: Expr.RecDef = δ.head,
            f: Func = new (class extends Func {
               __apply (v: Value): Value {
                  return eval_Trie(Env.concat(ρ, closeDefs(δ_0, ρ, δ_0)), def.σ).__apply(v)
               }
            })
      return extendEnv(closeDefs(δ_0, ρ, δ.tail), def.x.str, f)
   } else
   if (Nil.is(δ)) {
      return emptyEnv()
   } else {
      return absurd()
   }
}

// Repeatedly reeval_s subexpressions, so probably as slow as the previous implementation.
// Should be able to significantly speed up by memoisation.
export function eval_ (ρ: Env, e: Expr): Value {
   if (e instanceof Expr.ConstNum) {
      return num(e.val)
   } else
   if (e instanceof Expr.ConstStr) {
      return str(e.val)
   } else
   if (e instanceof Expr.Fun) {
      return eval_Trie(ρ, e.σ)
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
      return eval_Trie(ρ, e.σ).__apply(eval_(ρ, e.e))
   } else
   if (e instanceof Expr.LetRec) {
      const ρʹ: Env = closeDefs(e.δ, ρ, e.δ)
      return eval_(Env.concat(ρ, ρʹ), e.e)
   } else
   if (e instanceof Expr.MatchAs) {
      return eval_Trie(ρ, e.σ).__apply(eval_(ρ, e.e))
   } else {
      return absurd(`Unimplemented expression form: ${className(e)}.`)
   }
}

}
