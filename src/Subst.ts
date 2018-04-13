import { assert} from "./util/Core"
import { Env } from "./Env"
import { Eval } from "./Eval"
import { List } from "./List"
import { Expr, Trace, Traced, Value } from "./Syntax"

export function substSeq (es: List<Expr.Expr>, θ: Env): List<Traced> {

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
   } else {
      return assert(false)
   }
}