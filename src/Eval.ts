import { __nonNull, assert, as, make } from "./util/Core"
import { Env, EnvEntries, EnvEntry, closeDefs } from "./Env"
import { PrimBody, PrimResult } from "./Primitive"
import { Expr, Trace, TraceId, Traced, TracedId, Trie, Value } from "./Syntax"

export module Eval {

class EvalId extends Value.ValId {
   j: EnvEntries
   i: Expr.ExprId

   static make (j: EnvEntries, i: Expr.ExprId): EvalId {
      const this_: EvalId = make(EvalId, j, i)
      this_.j = j
      this_.i = i
      return this_
   }
}

class EvalTracedId extends TracedId {
   k: EvalId

   static make (k: EvalId): EvalTracedId {
      const this_: EvalTracedId = make(EvalTracedId, k)
      this_.k = k
      return this_
      
   }
}

class EvalTraceId extends TraceId {
   k: EvalId

   static make (k: EvalId): EvalTraceId {
      const this_: EvalTraceId = make(EvalTraceId, k)
      this_.k = k
      return this_
   }
}

class FunDemandId extends Trie.TrieId {
   k: EvalId   

   static make (k: EvalId): FunDemandId {
      const this_: FunDemandId = make(FunDemandId, k)
      this_.k = k
      return this_
   }
}

export type EvalResult<T> = [Traced, Env, T]    // tv, ρ, σv
type EvalResults = [Traced[], Env, Object]      // tvs, ρ, σv

function __result<T> (k: EvalId, t: Trace.Trace | null, v: Value.Value | null, ρ: Env, κ: T): EvalResult<T> {
   return [Traced.at(EvalTracedId.make(k), t, v), ρ, κ]
}

// Not capturing the polymorphic type of the nested trie κ (which has a depth of n >= 0).
function evalSeq (ρ: Env, κ: Object, es: Expr.Expr[]): EvalResults {
   if (es.length === 0) {
      return [[], Env.empty(), κ]
   } else {
      const σ: Trie.Trie<Object> = as(κ as Trie.Trie<Object>, Trie.Trie),
            [tv, ρʹ, κʹ]: EvalResult<Object> = eval_(ρ, es[0], σ),
            [tvs, ρʺ, κʺ]: EvalResults = evalSeq(ρ, κʹ, es.slice(1))
      return [[tv].concat(tvs), Env.concat(ρʹ, ρʺ), κʺ]
   }
}

// Output trace and value are unknown (null) iff σ is empty (i.e. a variable trie).
export function eval_<T> (ρ: Env, e: Expr.Expr, σ: Trie.Trie<T>): EvalResult<T> {
   const k: EvalId = EvalId.make(ρ.entries(), e.__id),
         kʹ: EvalTraceId = EvalTraceId.make(k)
   if (Trie.Var.is(σ)) {
      const entry: EnvEntry = EnvEntry.make(ρ, Expr.EmptyRecDefs.make(), e)
      return __result(k, null, null, Env.singleton(σ.x.str, entry), σ.body)
   } else {
      if (e instanceof Expr.Constr && Trie.Constr.is(σ) && σ.cases.has(e.ctr.str)) {
         const σʹ: Object = σ.cases.get(e.ctr.str)!,
               [tvs, ρʹ, κ]: EvalResults = evalSeq(ρ, σʹ, e.args)
         // have to cast κ without type information on constructor
         return __result(k, Trace.Empty.at(kʹ), Value.Constr.at(k, e.ctr, tvs), ρʹ, κ as T)
      } else
      if (e instanceof Expr.ConstInt && Trie.ConstInt.is(σ)) {
         return __result(k, Trace.Empty.at(kʹ), Value.ConstInt.at(k, e.val), Env.empty(), σ.body)
      } else
      if (e instanceof Expr.ConstStr && Trie.ConstStr.is(σ)) {
         return __result(k, Trace.Empty.at(kʹ), Value.ConstStr.at(k, e.val), Env.empty(), σ.body)
      } else
      if (e instanceof Expr.Fun && Trie.Fun.is(σ)) {
         return __result(k, Trace.Empty.at(kʹ), Value.Closure.at(k, ρ, Expr.EmptyRecDefs.make(), e), Env.empty(), σ.body)
      } else
      if (e instanceof Expr.PrimOp && Trie.Fun.is(σ)) {
         return __result(k, Trace.Empty.at(kʹ), e.op, Env.empty(), σ.body)
      } else
      if (e instanceof Expr.Var || e instanceof Expr.OpName) {
         const x: string = e instanceof Expr.OpName ? e.opName.str : e.ident.str
         if (!ρ.has(x)) {
            return assert(false, "Name not found.", x)
         } else {
            const {ρ: ρʹ, e: eʹ}: EnvEntry = ρ.get(x)!,
                  [tv, ρʺ, σv]: EvalResult<T> = eval_(ρʹ, eʹ, σ),
                  t: Trace.Trace = e instanceof Expr.OpName 
                     ? Trace.OpName.at(kʹ, e.opName, __nonNull(tv.trace))
                     : Trace.Var.at(kʹ, e.ident, __nonNull(tv.trace))
            return __result(k, t, tv.val, ρʺ, σv)
         }
      } else
      if (e instanceof Expr.Let) {
         const [tu, ρʹ, σu]: EvalResult<Expr.Expr> = eval_(ρ, e.e, e.σ),
               [tv, ρʺ, κ]: EvalResult<T> = eval_<T>(Env.concat(ρ, ρʹ), σu, σ)
         return __result(k, Trace.Let.at(kʹ, tu, __nonNull(tv.trace)), tv.val, ρʺ, κ)
      } else 
      // See 0.3.4 release notes for semantics.
      if (e instanceof Expr.LetRec) {
         const ρʹ: Env = closeDefs(e.δ, ρ, e.δ),
               [tv, ρʺ, σv]: EvalResult<T> = eval_<T>(ρʹ, e.e, σ)
         return __result(k, Trace.LetRec.at(kʹ, e.δ, __nonNull(tv.trace)), tv.val, ρʺ, σv)
      } else
      if (e instanceof Expr.MatchAs) {
         const [tu, ρʹ, σu]: EvalResult<Expr.Expr> = eval_(ρ, e.e, e.σ),
               [tv, ρʺ, κ]: EvalResult<T> = eval_<T>(Env.concat(ρ, ρʹ), σu, σ)
         return __result(k, Trace.Match.at(kʹ, tu, __nonNull(tv.trace)), tv.val, ρʺ, κ)
      } else
      if (e instanceof Expr.App) {
         const [tf, ,]: EvalResult<null> = eval_(ρ, e.func, Trie.Fun.at(FunDemandId.make(k), null)),
               f: Value.Value | null = tf.val
         if (f instanceof Value.Closure) {
            const [tu, ρ2, σʹu]: EvalResult<Expr.Expr> = eval_(ρ, e.arg, f.func.σ),
                  ρ1: Env = closeDefs(f.δ, f.ρ, f.δ),
                  [tv, ρʹ, σv]: EvalResult<T> = eval_<T>(Env.concat(ρ1, ρ2), σʹu, σ)
            return __result(k, Trace.App.at(kʹ, tf, tu, __nonNull(tv.trace)), tv.val, ρʹ, σv)
         } else
         if (f instanceof Value.PrimOp) {
            const [tu, , σʹu]: EvalResult<PrimBody<T>> = eval_(ρ, e.arg, f.σ),
                  [v, σv]: PrimResult<T> = σʹu(tu.val, σ)
            return __result(k, Trace.PrimApp.at(kʹ, tf, tu), v, Env.empty(), σv)
         } else {
            return assert(false, "Not a function.", f)
         }
      }
   }
   return assert(false, "Demand mismatch.", e, σ)
}

}
