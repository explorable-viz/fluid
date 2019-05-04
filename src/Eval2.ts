import { __nonNull, absurd, error } from "./util/Core"
import { Cons, List, Nil } from "./BaseTypes2"
import { DataType, datatypeFor } from "./DataType2"
import { Env, concat } from "./Env2"
import { Expr } from "./Expr2"
import { Func, State_Dyn, Value, construct } from "./ExplVal2"
import { interpretTrie } from "./Match2"

type InterpretExpr = (ρ: Env) => Value

// Repeatedly reinterprets subexpressions, so probably as slow as the previous implementation.
// Should be able to significantly speed up by memoisation.
export function interpret (e: Expr): InterpretExpr {
   if (e instanceof Expr.Var) {
      return (ρ: Env) => __nonNull(ρ[e.x])
   } else
   if (e instanceof Expr.App) {
      return (ρ: Env) => {
         const f: Value = interpret(e.func)(ρ)
         if (f instanceof Func) {
            const [ρʹ, eʹ]: [Env, Expr] = f.__apply(interpret(e.arg))
            // TODO: closeDefs
            return interpret(eʹ)(concat(ρ, ρʹ))
         } else {
            return error("Not a function")
         }
      }
   } else
   if (e instanceof Expr.Constr) {
      return (ρ: Env): Value => {
         const d: DataType = __nonNull(datatypeFor.get(e.ctr)),
               state: State_Dyn = {}
         let e̅: List<Expr> = e.args
         for (const f of d.fields) {
            if (Cons.is(e̅)) {
               state[f] = interpret(e̅.head)(ρ)
               e̅ = e̅.tail
            } else
            if (Nil.is(e̅)) {
               absurd()
            } 
         }
         return construct(new d.cls, state)
      }
   } else 
   if (e instanceof Expr.Fun) {
      return (ρ: Env) => interpretTrie(e.σ)
   } else
   if (e instanceof Expr.MatchAs) {
      return (ρ: Env): Value => {
         const [ρʹ, eʹ]: [Env, Expr] = interpretTrie(e.σ).__apply(interpret(e)(ρ))
         return interpret(eʹ)(concat(ρ, ρʹ))
      }
   } else {
      return absurd()
   }
}
