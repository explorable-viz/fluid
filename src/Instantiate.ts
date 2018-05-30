import { absurd } from "./util/Core"
import { List, Pair } from "./BaseTypes"
import { Env } from "./Env"
import { Eval, Runtime } from "./Eval"
import { Expr } from "./Expr"
import { Trace, Traced, Trie, Value } from "./Traced"

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
         const t: Trace = Trace.Let.at(i, instantiate(ρ)(e.e), instantiateTrie(ρ, e.σ) as Trie.Var<Traced>, null)
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

// TypeScript generics are borked. For example:
// 
// Type:                               is assignable to:
// Trie<Traced | Trie.Args<Traced>>    Trie<Traced>
// Expr.Trie<Expr.Trie.Args<Expr>>     Expr.Trie<Expr>

function instantiateArgs (ρ: Env, Π: Expr.Trie.Args<Expr>): Trie.Args<Traced> {
   if (Expr.Trie.End.is(Π)) {
      return Trie.End.make(instantiate(ρ)(Π.κ))
   } else
   if (Expr.Trie.Next.is(Π)) {
      return Trie.Next.make(instantiateTrie(ρ, Π.σ))
   } else {
      return absurd()
   }
}

function instantiateTrie (ρ: Env, σ: Expr.Trie<Expr>): Trie<Traced> {
   if (Expr.Trie.Var.is(σ)) {
      return Trie.Var.make(σ.x, instantiate(ρ)(σ.κ))
   } else
   if (Expr.Trie.ConstInt.is(σ)) {
      return Trie.ConstInt.make(instantiate(ρ)(σ.κ))
   } else
   if (Expr.Trie.ConstStr.is(σ)) {
      return Trie.ConstStr.make(instantiate(ρ)(σ.κ))
   } else
   if (Expr.Trie.Constr.is(σ)) {
      return Trie.Constr.make(σ.cases.map(
         ({ fst: ctr, snd: Π }: Pair<string, Expr.Trie.Args<Expr>>): Pair<string, Trie.Args<Traced>> => {
            return Pair.make(ctr, instantiateArgs(ρ, Π))
         })
      )
   } else
   if (Expr.Trie.Fun.is(σ)) {
      return Trie.Fun.make(instantiate(ρ)(σ.κ))
   } else {
      return absurd()
   }
}
