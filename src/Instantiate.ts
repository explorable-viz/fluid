import { absurd } from "./util/Core"
import { Pair } from "./BaseTypes"
import { Env } from "./Env"
import { Eval } from "./Eval"
import { Expr, Trace, Traced, Trie, Value } from "./Syntax"

export function instantiate (ρ: Env): (e: Expr.Expr) => Traced {
   return function (e: Expr.Expr): Traced {
      const i: Eval.Evaluand = Eval.Evaluand.make(ρ.entries(), e)
      if (e instanceof Expr.ConstInt) {
         return Traced.at(i, Trace.Empty.at(i), Value.ConstInt.at(i, e.val))
      } else
      if (e instanceof Expr.ConstStr) {
         return Traced.at(i, Trace.Empty.at(i), Value.ConstStr.at(i, e.val))
      } else
      if (e instanceof Expr.Constr) {
         return Traced.at(i, Trace.Empty.at(i), Value.Constr.at(i, e.ctr, e.args.map(instantiate(ρ))))
      } else
      if (e instanceof Expr.Fun) {
         // No need to use "unknown" environment here because we have ρ.
         return Traced.at(i, Trace.Empty.at(i), Value.Closure.at(i, ρ, instantiateTrie(ρ, e.σ)))
      } else
      if (e instanceof Expr.PrimOp) {
         return Traced.at(i, Trace.Empty.at(i), Value.PrimOp.at(i, e.op))
      } else
      if (e instanceof Expr.Var) {
         return Traced.at(i, Trace.Var.at(i, e.x, null), null)
      } else
      if (e instanceof Expr.Let) {
         const t: Trace = Trace.Let.at(i, instantiate(ρ)(e.e), instantiateTrie(ρ, e.σ) as Trie.Var<Traced>, null)
         return Traced.at(i, t, null)
      } else
      if (e instanceof Expr.LetRec) {
         const t: Trace = Trace.LetRec.at(i, e.δ.map(def => Trace.RecDef.at(i, def.x, instantiate(ρ)(def.e))), instantiate(ρ)(e.e))
         return Traced.at(i, t, null)
      } else
      if (e instanceof Expr.MatchAs) {
         return Traced.at(i, Trace.MatchAs.at(i, instantiate(ρ)(e.e), instantiateTrie(ρ, e.σ), null), null)
      } else
      if (e instanceof Expr.App) {
         return Traced.at(i, Trace.App.at(i, instantiate(ρ)(e.func), instantiate(ρ)(e.arg), null), null)
      } else
      if (e instanceof Expr.PrimApp) {
         return Traced.at(i, Trace.PrimApp.at(i, instantiate(ρ)(e.e1), e.opName, instantiate(ρ)(e.e2)), null)
      } else {
         return absurd()
      }
   }
}

// Can't give this a more specific type without type variables of kind * -> *.
function instantiateTrie (ρ: Env, σ: Trie<Expr>): Trie<Traced> {
   if (Trie.Var.is(σ)) {
      return Trie.Var.make(σ.x, instantiate(ρ)(σ.body))
   } else
   if (Trie.ConstInt.is(σ)) {
      return Trie.ConstInt.make(instantiate(ρ)(σ.body))
   } else
   if (Trie.ConstStr.is(σ)) {
      return Trie.ConstStr.make(instantiate(ρ)(σ.body))
   } else
   if (Trie.Constr.is(σ)) {
      return Trie.Constr.make(σ.cases.map(
         ({ fst: ctr, snd: body }: Pair<string, Expr>) => Pair.make(ctr, instantiate(ρ)(body)))
      )
   } else
   if (Trie.Fun.is(σ)) {
      return Trie.Fun.make(instantiate(ρ)(σ.body))
   } else {
      return absurd()
   }
}
