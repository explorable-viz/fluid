import { assert} from "./util/Core"
import { Env } from "./Env"
import { Eval } from "./Eval"
import { Cons, List, Nil } from "./List"
import { Expr, Trace, Traced, Value } from "./Syntax"

export function substSeq (es: List<Expr.Expr>, θ: Env): List<Traced> {
   if (Cons.is(es)) {
      return Cons.make(subst(es.head, θ), substSeq(es.tail, θ))
   } else
   if (Nil.is(es)) {
      return Nil.make()
   } else {
      return assert(false)
   }
}

export function subst (e: Expr.Expr, θ: Env): Traced {
   const i: Eval.Evaluand = Eval.Evaluand.make(θ.entries(), e)
   if (e instanceof Expr.ConstInt) {
      return Traced.at(i, Trace.Empty.at(i), Value.ConstInt.at(i, e.val))
   } else
   if (e instanceof Expr.ConstStr) {
      return Traced.at(i, Trace.Empty.at(i), Value.ConstStr.at(i, e.val))
   } else
   if (e instanceof Expr.Constr) {
      return Traced.at(i, Trace.Empty.at(i), Value.Constr.at(i, e.ctr, substSeq(e.args, θ)))
   } else
   if (e instanceof Expr.Fun) {
      return Traced.at(i, Trace.Empty.at(i), Value.Closure.at(i, θ, e))
   } else
   if (e instanceof Expr.PrimOp) {
      return Traced.at(i, Trace.Empty.at(i), e.op)
   } else
   if (e instanceof Expr.Var) {
      return Traced.at(i, Trace.Var.at(i, e.ident, null), null)
   } else
   if (e instanceof Expr.OpName) {
      return Traced.at(i, Trace.OpName.at(i, e.opName, null), null)
   } else
   if (e instanceof Expr.Let) {
      return Traced.at(i, Trace.Let.at(i, subst(e.e, θ), null), null)
   } else
   if (e instanceof Expr.LetRec) {
      return Traced.at(i, Trace.LetRec.at(i, e.δ, subst(e.e, θ).trace!), null)
   } else
   if (e instanceof Expr.MatchAs) {
      return Traced.at(i, Trace.Match.at(i, subst(e.e, θ), null), null)
   } else
   if (e instanceof Expr.App) {
      return Traced.at(i, Trace.App.at(i, subst(e.func, θ), subst(e.arg, θ), null), null)
   } else {
      return assert(false)
   }
}
