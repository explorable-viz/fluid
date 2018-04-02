import { zip } from "./util/Array"
import { assert, as, make } from "./util/Core"
import { Env, EnvEntry, EnvId, get, has } from "./Env"
import { Id } from "./Memo"
import { PrimBody, PrimResult } from "./Primitive"
import { Expr, Trace, TraceId, Traced, TracedId, Trie, Value } from "./Syntax"

export module Eval {

class EvalId extends Value.ValId {
   j: EnvId
   i: Expr.ExprId

   static make (j: EnvId, i: Expr.ExprId): EvalId {
      const this_: EvalId = make(EvalId, j, i)
      this_.j = j
      this_.i = i
      return this_
   }
}

export class EvalTracedId extends TracedId {
   k: EvalId

   static make (k: EvalId): EvalTracedId {
      const this_: EvalTracedId = make(EvalTracedId, k)
      this_.k = k
      return this_
   }
}

export class EvalTraceId extends TraceId {
   k: EvalId

   static make (k: EvalId): EvalTraceId {
      const this_: EvalTraceId = make(EvalTraceId, k)
      this_.k = k
      return this_
   }
}

export type EvalResult<T> = [Traced, Env, T]    // tv, ρ, σv
type EvalResults = [Traced[], Env, Object]      // tvs, ρ, σv

function __result<T> (α: EvalId, t: Trace.Trace, v: Value.Value | null, ρ: Env, κ: T): EvalResult<T> {
   return [Traced.at(EvalTracedId.make(α), t, v), ρ, κ]
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

export function eval_<T> (ρ: Env, σ: Trie.Trie<T>, e: Expr.Expr): EvalResult<T> {
   const k: EvalId = EvalId.make(ρ.__id, e.__id),
         kʹ: EvalTraceId = EvalTraceId.make(k)
   if (Trie.Var.is(σ)) {
      return __result(k, Trace.Empty.at(EvalTraceId.make(k)), null, [[σ.x.str, {ρ, δ: [], e}]], σ.body)
   } else {
      if (e instanceof Expr.Constr && Trie.Constr.is(σ) && σ.cases.has(e.ctr.str)) {
         const σʹ: Object = σ.cases.get(e.ctr.str)!,
               [tvs, ρʹ, κ]: EvalResults = evalSeq(ρ, σʹ, e.args)
         // have to cast κ without type information on constructor
         return __result(k, Trace.Empty.at(kʹ), Value.Constr.at(k, e.ctr, tvs), ρʹ, κ as T)
      } else
      if (e instanceof Expr.ConstInt && Trie.ConstInt.is(σ)) {
         return __result(k, Trace.Empty.at(kʹ), Value.ConstInt.at(k, e.val), [], σ.body)
      } else
      if (e instanceof Expr.ConstStr && Trie.ConstStr.is(σ)) {
         return __result(k, Trace.Empty.at(kʹ), Value.ConstStr.at(k, e.val), [], σ.body)

      } else
      if (e instanceof Expr.Fun && Trie.Fun.is(σ)) {
         const v: Value.Closure = Value.Closure.at(k, ρ, [], e)
         return __result(k, Trace.Empty.at(kʹ), v, [], σ.body)
      } else
      if (e instanceof Expr.PrimOp && Trie.Fun.is(σ)) {
         return __result(k, Trace.Empty.at(kʹ), e.op, [], σ.body)
      } else
      if (e instanceof Expr.OpName || e instanceof Expr.Var) {
         const x: string = e instanceof Expr.OpName ? e.opName.str : e.ident.str
         if (!has(ρ, x)) {
            return assert(false, "Name not found.", x)
         } else {
            const cls: EnvEntry = get(ρ, x)!,
                  [tv, ρʺ, σv]: EvalResult<T> = eval_(cls.ρ, σ, cls.e),
                  t: Trace.Trace = e instanceof Expr.OpName 
                     ? Trace.OpName.at(kʹ, e.opName, tv.trace)
                     : Trace.Var.at(kʹ, e.ident, tv.trace)
            return __result(k, t, tv.val, ρʺ, σv)
         }
      } else
      if (e instanceof Expr.Let) {
         const [tu, ρʹ, σu]: EvalResult<Expr.Expr> = eval_(ρ, e.σ, e.e),
               [tv, ρʺ, κ]: EvalResult<T> = eval_<T>(ρ.concat(ρʹ), σ, σu)
         return __result(k, Trace.Let.at(kʹ, tu, tv.trace), tv.val, ρʺ, κ)
      } else 
      // See 0.3.4 release notes for semantics.
      if (e instanceof Expr.LetRec) {
         const fs: EnvEntry[] = e.δ.map(def => new EnvEntry(ρ, e.δ, def.func)),
               ρʹ: Env = ρ.concat(zip(e.δ.map(def => def.name.str), fs)),
               [tv, ρʺ, σv]: EvalResult<T> = eval_<T>(ρʹ, σ, e.e)
         return __result(k, Trace.LetRec.at(kʹ, e.δ, tv.trace), tv.val, ρʺ, σv)
      } else
      if (e instanceof Expr.MatchAs) {
         const [tu, ρʹ, σu]: EvalResult<Expr.Expr> = eval_(ρ, e.σ, e.e),
               [tv, ρʺ, κ]: EvalResult<T> = eval_<T>(ρ.concat(ρʹ), σ, σu)
         return __result(k, Trace.Match.at(kʹ, tu, tv.trace), tv.val, ρʺ, κ)
      } else
      if (e instanceof Expr.App) {
         const [tf, ,]: EvalResult<null> = eval_<null>(ρ, Trie.Fun.at(keyP(α, "1"), null), e.func),
               f: Value.Value | null = tf.val
         if (f instanceof Value.Closure) {
            const [tu, ρ2, σʹu]: EvalResult<Expr.Expr> = eval_(ρ, f.func.σ, e.arg),
                  fs: EnvEntry[] = f.δ.map(def => new EnvEntry(f.ρ, f.δ, def.func)),
                  ρ1: Env = f.ρ.concat(zip(f.δ.map(def => def.name.str), fs)),
                  [tv, ρʹ, σv]: EvalResult<T> = eval_<T>(ρ1.concat(ρ2), σ, σʹu)
            return __result(k, Trace.App.at(kʹ, tf, tu, tv.trace), tv.val, ρʹ, σv)
         } else
         if (f instanceof Value.PrimOp) {
            const [tu, , σʹu]: EvalResult<PrimBody<T>> = eval_(ρ, f.σ, e.arg),
                  [v, σv]: PrimResult<T> = σʹu(tu.val, σ)
            return __result(k, Trace.PrimApp.at(kʹ, tf, tu), v, [], σv)
         } else {
            return assert(false, "Not a function.", f)
         }
      }
   }
   return assert(false, "Demand mismatch.")
}

}
