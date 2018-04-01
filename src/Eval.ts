import { zip } from "./util/Array"
import { assert, as } from "./util/Core"
import { Env, EnvEntry, entries, get, has } from "./Env"
import { Addr, keyA, keyP } from "./Memo"
import { PrimBody, PrimResult } from "./Primitive"
import { Expr, Trace, Traced, Trie, Value } from "./Syntax"

export module Eval {

export type EvalResult<T> = [Traced, Env, T]    // tv, ρ, σv
type EvalResults = [Traced[], Env, Object]      // tvs, ρ, σv

function __result<T> (α: Addr, t: Trace.Trace, v: Value.Value | null, ρ: Env, κ: T): EvalResult<T> {
   return [Traced.at(α, t, v), ρ, κ]
}

// Don't think I capture the polymorphic type of the nested trie κ (which has a depth of n >= 0).
function evalSeq (ρ: Env, κ: Object, es: Expr.Expr[]): EvalResults {
   if (es.length === 0) {
      return [[], [], κ]
   } else {
      const σ: Trie.Trie<Object> = as(κ as Trie.Trie<Object>, Trie.Trie),
            [tv, ρʹ, κʹ]: EvalResult<Object> = eval_(ρ, σ, es[0]),
            [tvs, ρʺ, κʺ]: EvalResults = evalSeq(ρ, κʹ, es.slice(1))
      return [[tv].concat(tvs), ρʹ.concat(ρʺ), κʺ]
   }
}

function traceOf (α: Addr): Addr {
   return keyP(α, "trace")
}

function valOf (α: Addr): Addr {
   return keyP(α, "val")
}

export function eval_<T> (ρ: Env, σ: Trie.Trie<T>, e: Expr.Expr): EvalResult<T> {
   const α: Addr = keyA(eval_, e, ...entries(ρ))
   if (Trie.Var.is(σ)) {
      return __result(α, Trace.Empty.at(α), null, [[σ.x.str, {ρ, δ: [], e}]], σ.body)
   } else {
      if (e instanceof Expr.Constr && Trie.Constr.is(σ) && σ.cases.has(e.ctr.str)) {
         const σʹ: Object = σ.cases.get(e.ctr.str)!,
               [tvs, ρʹ, κ]: EvalResults = evalSeq(ρ, σʹ, e.args)
         // have to cast κ without type information on constructor
         return __result(α, Trace.Empty.at(traceOf(α)), Value.Constr.at(valOf(α), e.ctr, tvs), ρʹ, κ as T)
      } else
      if (e instanceof Expr.ConstInt && Trie.ConstInt.is(σ)) {
         return __result(α, Trace.Empty.at(traceOf(α)), Value.ConstInt.at(valOf(α), e.val), [], σ.body)
      } else
      if (e instanceof Expr.ConstStr && Trie.ConstStr.is(σ)) {
         return __result(α, Trace.Empty.at(traceOf(α)), Value.ConstStr.at(valOf(α), e.val), [], σ.body)

      } else
      if (e instanceof Expr.Fun && Trie.Fun.is(σ)) {
         const v: Value.Closure = Value.Closure.at(valOf(α), ρ, [], e)
         return __result(α, Trace.Empty.at(traceOf(α)), v, [], σ.body)
      } else
      if (e instanceof Expr.PrimOp && Trie.Fun.is(σ)) {
         return __result(α, Trace.Empty.at(traceOf(α)), e.op, [], σ.body)
      } else
      if (e instanceof Expr.OpName || e instanceof Expr.Var) {
         const x: string = e instanceof Expr.OpName ? e.opName.str : e.ident.str
         if (!has(ρ, x)) {
            return assert(false, "Name not found.", x)
         } else {
            const cls: EnvEntry = get(ρ, x)!,
                  [tv, ρʺ, σv]: EvalResult<T> = eval_(cls.ρ, σ, cls.e),
                  t: Trace.Trace = e instanceof Expr.OpName 
                     ? Trace.OpName.at(traceOf(α), e.opName, tv.trace)
                     : Trace.Var.at(traceOf(α), e.ident, tv.trace)
            return __result(α, t, tv.val, ρʺ, σv)
         }
      } else
      if (e instanceof Expr.Let) {
         const [tu, ρʹ, σu]: EvalResult<Expr.Expr> = eval_(ρ, e.σ, e.e),
               [tv, ρʺ, κ]: EvalResult<T> = eval_<T>(ρ.concat(ρʹ), σ, σu)
         return __result(α, Trace.Let.at(traceOf(α), tu, tv.trace), tv.val, ρʺ, κ)
      } else 
      // See 0.3.4 release notes for semantics.
      if (e instanceof Expr.LetRec) {
         const fs: EnvEntry[] = e.δ.map(def => new EnvEntry(ρ, e.δ, def.func)),
               ρʹ: Env = ρ.concat(zip(e.δ.map(def => def.name.str), fs)),
               [tv, ρʺ, σv]: EvalResult<T> = eval_<T>(ρʹ, σ, e.e)
         return __result(α, Trace.LetRec.at(traceOf(α), e.δ, tv.trace), tv.val, ρʺ, σv)
      } else
      if (e instanceof Expr.MatchAs) {
         const [tu, ρʹ, σu]: EvalResult<Expr.Expr> = eval_(ρ, e.σ, e.e),
               [tv, ρʺ, κ]: EvalResult<T> = eval_<T>(ρ.concat(ρʹ), σ, σu)
         return __result(α, Trace.Match.at(traceOf(α), tu, tv.trace), tv.val, ρʺ, κ)
      } else
      if (e instanceof Expr.App) {
         const [tf, ,]: EvalResult<null> = eval_<null>(ρ, Trie.Fun.at(keyP(α, "1"), null), e.func),
               f: Value.Value | null = tf.val
         if (f instanceof Value.Closure) {
            const [tu, ρ2, σʹu]: EvalResult<Expr.Expr> = eval_(ρ, f.func.σ, e.arg),
                  fs: EnvEntry[] = f.δ.map(def => new EnvEntry(f.ρ, f.δ, def.func)),
                  ρ1: Env = f.ρ.concat(zip(f.δ.map(def => def.name.str), fs)),
                  [tv, ρʹ, σv]: EvalResult<T> = eval_<T>(ρ1.concat(ρ2), σ, σʹu)
            return __result(α, Trace.App.at(traceOf(α), tf, tu, tv.trace), tv.val, ρʹ, σv)
         } else
         if (f instanceof Value.PrimOp) {
            const [tu, , σʹu]: EvalResult<PrimBody<T>> = eval_(ρ, f.σ, e.arg),
                  [v, σv]: PrimResult<T> = σʹu(tu.val, σ)
            return __result(α, Trace.PrimApp.at(traceOf(α), tf, tu), v, [], σv)
         } else {
            return assert(false, "Not a function.", f)
         }
      }
   }
   return assert(false, "Demand mismatch.")
}

}
