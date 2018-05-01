import { absurd, assert } from "./util/Core"
import { List, Pair } from "./BaseTypes"
import { Env } from "./Env"
import { Eval, Runtime } from "./Eval"
import { Expr, Trace, Traced, Trie, Kont, Value } from "./Syntax"

export function instantiate (ρ: Env): (e: Expr) => Traced {
   return function (e: Expr.Expr): Traced {
      const i: Runtime<Expr> = Runtime.make(ρ.entries(), e)
      if (e instanceof Expr.ConstInt) {
         return Traced.make(Trace.Empty.at(i), Value.ConstInt.at(i, e.val))
      } else
      if (e instanceof Expr.ConstStr) {
         return Traced.make(Trace.Empty.at(i), Value.ConstStr.at(i, e.val))
      } else
      if (e instanceof Expr.Constr) {
         // Parser ensures constructors agree with constructor signatures.
         return Traced.make(Trace.Empty.at(i), Value.Constr.at(i, e.ctr, e.args.map(instantiate(ρ))))
      } else
      if (e instanceof Expr.Fun) {
         // No need to use "unknown" environment here because we have ρ.
         return Traced.make(Trace.Empty.at(i), Value.Closure.at(i, ρ, instantiateTrie(ρ, e.σ)))
      } else
      if (e instanceof Expr.PrimOp) {
         return Traced.make(Trace.Empty.at(i), Value.PrimOp.at(i, e.op))
      } else
      if (e instanceof Expr.Var) {
         return Traced.make(Trace.Var.at(i, e.x, null), null)
      } else
      if (e instanceof Expr.Let) {
         // Trace must still be null even though I know "statically" which branch will be taken.
         const t: Trace = Trace.Let.at(i, instantiate(ρ)(e.e), instantiateTrie(ρ, e.σ) as Trie.Var, null)
         return Traced.make(t, null)
      } else
      if (e instanceof Expr.LetRec) {
         const δ: List<Trace.RecDef> = e.δ.map(def => {
            const j: Runtime<Expr.RecDef> = Runtime.make(ρ.entries(), def)
            return Trace.RecDef.at(j, def.x, instantiate(ρ)(def.e))
         })
         const t: Trace = Trace.LetRec.at(i, δ, instantiate(Eval.closeDefs(δ, ρ, δ))(e.e))
         return Traced.make(t, null)
      } else
      if (e instanceof Expr.MatchAs) {
         return Traced.make(Trace.MatchAs.at(i, instantiate(ρ)(e.e), instantiateTrie(ρ, e.σ), null), null)
      } else
      if (e instanceof Expr.App) {
         return Traced.make(Trace.App.at(i, instantiate(ρ)(e.func), instantiate(ρ)(e.arg), null), null)
      } else
      if (e instanceof Expr.PrimApp) {
         return Traced.make(Trace.PrimApp.at(i, instantiate(ρ)(e.e1), e.opName, instantiate(ρ)(e.e2)), null)
      } else {
         return absurd()
      }
   }
}

// Turns an "expression" trie body into a "traced value" trie body.
function instantiateKont (ρ: Env, κ: Kont): Kont {
   if (κ instanceof Trie.Trie) {
      return instantiateTrie(ρ, κ)
   } else
   if (κ instanceof Expr.Expr) {
      return instantiate(ρ)(κ)
   } else {
      return assert(false)
   }
}

// Turns trie of expressions into trie of traced values.
function instantiateTrie (ρ: Env, σ: Trie): Trie {
   if (σ instanceof Trie.Var) {
      return Trie.Var.make(σ.x, instantiateKont(ρ, σ.body))
   } else
   if (σ instanceof Trie.ConstInt) {
      return Trie.ConstInt.make(instantiateKont(ρ, σ.body))
   } else
   if (σ instanceof Trie.ConstStr) {
      return Trie.ConstStr.make(instantiateKont(ρ, σ.body))
   } else
   if (σ instanceof Trie.Constr) {
      return Trie.Constr.make(σ.cases.map(
         ({ fst: ctr, snd: κ }: Pair<string, Kont>): Pair<string, Kont> => {
            return Pair.make(ctr, instantiateKont(ρ, κ))
         })
      )
   } else
   if (σ instanceof Trie.Fun) {
      return Trie.Fun.make(instantiateKont(ρ, σ.body))
   } else {
      return absurd()
   }
}
