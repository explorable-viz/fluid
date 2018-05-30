import { absurd } from "./util/Core"
import { List, Pair } from "./BaseTypes"
import { Env } from "./Env"
import { Eval, Runtime } from "./Eval"
import { Expr } from "./Expr"
import { Trace, Traced, Trie, Kont, Value } from "./Traced"

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
         const t: Trace = Trace.Let.at(i, instantiate(ρ)(e.e), instantiateTrie(ρ, e.σ) as Trie.Var<Expr>, null)
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

function instantiateKont<K> (ρ: Env, κ: K): Kont {
   if (κ instanceof Expr.Trie.Trie) {
      return instantiateTrie(ρ, κ)
   } else
   if (κ instanceof Expr.Expr) {
      return instantiate(ρ)(κ)
   } else
   if (κ instanceof Expr.Trie.Args) {
      return instantiateArgs(ρ, κ)
   } else {
      return absurd()
   }
}

function instantiateArgs<K> (ρ: Env, Π: Expr.Trie.Args<K>): Trie.Args<K> {
   if (Π instanceof Expr.Trie.End) {
      return Trie.End.make(instantiateKont(ρ, Π.κ))
   } else
   if (Π instanceof Expr.Trie.Next) {
      return Trie.Next.make(instantiateTrie(ρ, Π.σ))
   } else {
      return absurd()
   }
}

function instantiateTrie<K> (ρ: Env, σ: Expr.Trie<K>): Trie<K> {
   if (σ instanceof Expr.Trie.Var) {
      return Trie.Var.make(σ.x, instantiateKont(ρ, σ.κ))
   } else
   if (σ instanceof Expr.Trie.ConstInt) {
      return Trie.ConstInt.make(instantiateKont(ρ, σ.κ))
   } else
   if (σ instanceof Expr.Trie.ConstStr) {
      return Trie.ConstStr.make(instantiateKont(ρ, σ.κ))
   } else
   if (σ instanceof Expr.Trie.Constr) {
      return Trie.Constr.make(σ.cases.map(
         ({ fst: ctr, snd: Π }: Pair<string, Expr.Trie.Args<K>>): Pair<string, Trie.Args<K>> => {
            return Pair.make(ctr, instantiateArgs(ρ, Π))
         })
      )
   } else
   if (σ instanceof Expr.Trie.Fun) {
      return Trie.Fun.make(instantiateKont(ρ, σ.κ))
   } else {
      return absurd()
   }
}
