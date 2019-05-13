import { __nonNull, absurd, className, error } from "./util/Core"
import { Cons, List, Nil, nil } from "./BaseTypes2"
import { ctrFor } from "./DataType2"
import { Env, concat, emptyEnv, extendEnv, get, has } from "./Env2"
import { Expl, ExplValue, explValue } from "./ExplValue2"
import { Expr } from "./Expr2"
import { Closure, closure } from "./Func2"
import { evalTrie } from "./Match2"
import { UnaryOp, BinaryOp, binaryOps } from "./Primitive2"
import { Value, _, make } from "./Value2"

export module Eval {

// Environments are snoc-lists, so this (inconsequentially) reverses declaration order.
export function closeDefs (δ_0: List<Expr.RecDef>, ρ: Env, δ: List<Expr.RecDef>): Env {
   if (Cons.is(δ)) {
      const { σ, x }: Expr.RecDef = δ.head
      return extendEnv(closeDefs(δ_0, ρ, δ.tail), x, closure(ρ, δ_0, σ))
   } else
   if (Nil.is(δ)) {
      return emptyEnv()
   } else {
      return absurd()
   }
}

export function eval_ (ρ: Env, e: Expr): ExplValue {
   if (e instanceof Expr.ConstNum) {
      return explValue(Expl.empty(), e.val)
   } else
   if (e instanceof Expr.ConstStr) {
      return explValue(Expl.empty(), e.val)
   } else
   if (e instanceof Expr.Fun) {
      return explValue(Expl.empty(), closure(ρ, nil(), e.σ))
   } else
   if (e instanceof Expr.PrimOp) {
      return explValue(Expl.empty(), e.op)
   } else
   if (e instanceof Expr.Constr) {
      let v̅: Value[] = e.args.toArray().map((e: Expr) => eval_(ρ, e).v)
      return explValue(Expl.empty(), make(ctrFor(e.ctr).C, ...v̅))
   } else 
   if (e instanceof Expr.Var) {
      if (has(ρ, e.x)) { 
         return explValue(Expl.var_(e.x), get(ρ, e.x)!)
      } else {
         return error(`Variable '${e.x.val}' not found.`)
      }
   } else
   if (e instanceof Expr.App) {
      const tf: ExplValue = eval_(ρ, e.func),
            tu: ExplValue = eval_(ρ, e.arg),
            f: Value = tf.v
      if (f instanceof Closure) {
         const [ρʹ, eʹ]: [Env, Expr] = evalTrie(f.σ).__apply(tu.v),
               tv: ExplValue = eval_(concat(f.ρ, concat(closeDefs(f.δ, f.ρ, f.δ), ρʹ)), eʹ)
         return explValue(Expl.app(tf, tu, tv), tv.v)
      } else 
      if (f instanceof UnaryOp) {
         return explValue(Expl.unaryApp(tf, tu), f.op(tu.v))
      } else {
         return error(`Cannot apply ${className(f)}`)
      }
   } else
   // Operators (currently all binary) are "syntax", rather than names.
   if (e instanceof Expr.BinaryApp) {
      if (binaryOps.has(e.opName.val)) {
         const op: BinaryOp = binaryOps.get(e.opName.val)!, // opName lacks annotations
               [tv1, tv2]: [ExplValue, ExplValue] = [eval_(ρ, e.e1), eval_(ρ, e.e2)]
         return explValue(Expl.binaryApp(tv1, e.opName, tv2), op.op(tv1.v, tv2.v))
      } else {
         return error(`Operator ${e.opName.val} not found.`)
      }
   } else
   if (e instanceof Expr.Let) {
      const tu: ExplValue = eval_(ρ, e.e),
            [ρʹ, eʹ]: [Env, Expr] = evalTrie(e.σ).__apply(tu.v),
            tv: ExplValue = eval_(concat(ρ, ρʹ), eʹ)
      return explValue(Expl.let_(tu, tv), tv.v)
   } else
   if (e instanceof Expr.LetRec) {
      const ρʹ: Env = closeDefs(e.δ, ρ, e.δ),
            tv: ExplValue = eval_(concat(ρ, ρʹ), e.e)
      return explValue(Expl.letRec(e.δ, ρʹ, tv), tv.v)
   } else
   if (e instanceof Expr.MatchAs) {
      const tu: ExplValue = eval_(ρ, e.e),
            [ρʹ, eʹ]: [Env, Expr] = evalTrie(e.σ).__apply(tu.v),
            tv: ExplValue = eval_(concat(ρ, ρʹ), eʹ)
      return explValue(Expl.matchAs(tu, tv), tv.v)
   } else {
      return absurd(`Unimplemented expression form: ${className(e)}.`)
   }
}

}
