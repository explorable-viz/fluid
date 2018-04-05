import { zip } from "./util/Array"
import { __nonNull, assert, as, make } from "./util/Core"
import { Env, EnvId, EnvEntry, EnvEntryId } from "./Env"
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

export type EvalResult<T> = [Traced, Env, EnvId, T]    // tv, ρ, j, σv
type EvalResults = [Traced[], Env, EnvId, Object]      // tvs, ρ, j, σv

function __result<T> (k: EvalId, t: Trace.Trace | null, v: Value.Value | null, ρ: Env, j: EnvId, κ: T): EvalResult<T> {
   return [Traced.at(EvalTracedId.make(k), t, v), ρ, j, κ]
}

// Not capturing the polymorphic type of the nested trie κ (which has a depth of n >= 0).
function evalSeq (ρ: Env, j: EnvId, κ: Object, es: Expr.Expr[]): EvalResults {
   if (es.length === 0) {
      return [[], Env.empty(), EnvId.empty(), κ]
   } else {
      const σ: Trie.Trie<Object> = as(κ as Trie.Trie<Object>, Trie.Trie),
            [tv, ρʹ, jʹ, κʹ]: EvalResult<Object> = eval_(ρ, j, σ, es[0]),
            [tvs, ρʺ, jʺ, κʺ]: EvalResults = evalSeq(ρ, j, κʹ, es.slice(1))
      return [[tv].concat(tvs), Env.concat(ρʹ, ρʺ), EnvId.concat(jʹ, jʺ), κʺ]
   }
}

// Output trace and value are unknown (null) iff σ is empty (i.e. a variable trie).
export function eval_<T> (ρ: Env, j: EnvId, σ: Trie.Trie<T>, e: Expr.Expr): EvalResult<T> {
   const k: EvalId = EvalId.make(j, e.__id),
         kʹ: EvalTraceId = EvalTraceId.make(k)
   if (Trie.Var.is(σ)) {
      const d: Expr.RecDefsId = Expr.RecDefsId.make(e.__id),
            entry: EnvEntry = new EnvEntry(ρ, j, Expr.RecDefs.at(d, []), e),
            l: EnvEntryId = EnvEntryId.make(j, d, e.__id)
      return __result(k, null, null, Env.singleton(σ.x.str, entry), EnvId.singleton(l), σ.body)
   } else {
      if (e instanceof Expr.Constr && Trie.Constr.is(σ) && σ.cases.has(e.ctr.str)) {
         const σʹ: Object = σ.cases.get(e.ctr.str)!,
               [tvs, ρʹ, jʹ, κ]: EvalResults = evalSeq(ρ, j, σʹ, e.args)
         // have to cast κ without type information on constructor
         return __result(k, Trace.Empty.at(kʹ), Value.Constr.at(k, e.ctr, tvs), ρʹ, jʹ, κ as T)
      } else
      if (e instanceof Expr.ConstInt && Trie.ConstInt.is(σ)) {
         return __result(k, Trace.Empty.at(kʹ), Value.ConstInt.at(k, e.val), Env.empty(), EnvId.empty(), σ.body)
      } else
      if (e instanceof Expr.ConstStr && Trie.ConstStr.is(σ)) {
         return __result(k, Trace.Empty.at(kʹ), Value.ConstStr.at(k, e.val), Env.empty(), EnvId.empty(), σ.body)
      } else
      if (e instanceof Expr.Fun && Trie.Fun.is(σ)) {
         const d: Expr.RecDefsId = Expr.RecDefsId.make(e.__id)
         return __result(k, Trace.Empty.at(kʹ), Value.Closure.at(k, ρ, j, Expr.RecDefs.at(d, []), e), Env.empty(), EnvId.empty(), σ.body)
      } else
      if (e instanceof Expr.PrimOp && Trie.Fun.is(σ)) {
         return __result(k, Trace.Empty.at(kʹ), e.op, Env.empty(), EnvId.empty(), σ.body)
      } else
      if (e instanceof Expr.Var || e instanceof Expr.OpName) {
         const x: string = e instanceof Expr.OpName ? e.opName.str : e.ident.str
         if (!ρ.has(x)) {
            return assert(false, "Name not found.", x)
         } else {
            const {ρ: ρʹ, j: jʹ, e: eʹ}: EnvEntry = ρ.get(x)!,
                  [tv, ρʺ, jʺ, σv]: EvalResult<T> = eval_(ρʹ, jʹ, σ, eʹ),
                  t: Trace.Trace = e instanceof Expr.OpName 
                     ? Trace.OpName.at(kʹ, e.opName, __nonNull(tv.trace))
                     : Trace.Var.at(kʹ, e.ident, __nonNull(tv.trace))
            return __result(k, t, tv.val, ρʺ, jʺ, σv)
         }
      } else
      if (e instanceof Expr.Let) {
         const [tu, ρʹ, jʹ, σu]: EvalResult<Expr.Expr> = eval_(ρ, j, e.σ, e.e),
               [tv, ρʺ, jʺ, κ]: EvalResult<T> = eval_<T>(Env.concat(ρ, ρʹ), EnvId.concat(j, jʹ), σ, σu)
         return __result(k, Trace.Let.at(kʹ, tu, __nonNull(tv.trace)), tv.val, ρʺ, jʺ, κ)
      } else 
      // See 0.3.4 release notes for semantics.
      if (e instanceof Expr.LetRec) {
         const fs: EnvEntry[] = e.δ.defs.map(def => new EnvEntry(ρ, j, e.δ, def.def)),
               ρʹ: Env = Env.extend(ρ, zip(e.δ.defs.map(def => def.x.str), fs)),
               jʹ: EnvId = EnvId.extend(j, fs.map((fʹ: EnvEntry) => EnvEntryId.make(fʹ.j, fʹ.δ.__id, fʹ.e.__id))),
               [tv, ρʺ, jʺ, σv]: EvalResult<T> = eval_<T>(ρʹ, jʹ, σ, e.e)
         return __result(k, Trace.LetRec.at(kʹ, e.δ, __nonNull(tv.trace)), tv.val, ρʺ, jʺ, σv)
      } else
      if (e instanceof Expr.MatchAs) {
         const [tu, ρʹ, jʹ, σu]: EvalResult<Expr.Expr> = eval_(ρ, j, e.σ, e.e),
               [tv, ρʺ, jʺ, κ]: EvalResult<T> = eval_<T>(Env.concat(ρ, ρʹ), EnvId.concat(j, jʹ), σ, σu)
         return __result(k, Trace.Match.at(kʹ, tu, __nonNull(tv.trace)), tv.val, ρʺ, jʺ, κ)
      } else
      if (e instanceof Expr.App) {
         const [tf, ,]: EvalResult<null> = eval_(ρ, j, Trie.Fun.at(FunDemandId.make(k), null), e.func),
               f: Value.Value | null = tf.val
         if (f instanceof Value.Closure) {
            const [tu, ρ2, j2, σʹu]: EvalResult<Expr.Expr> = eval_(ρ, j, f.func.σ, e.arg),
                  fs: EnvEntry[] = f.δ.defs.map(def => new EnvEntry(f.ρ, f.j, f.δ, def.def)),
                  ρ1: Env = Env.extend(f.ρ, zip(f.δ.defs.map(def => def.x.str), fs)),
                  j1: EnvId = EnvId.extend(f.j, fs.map((fʹ: EnvEntry) => EnvEntryId.make(fʹ.j, fʹ.δ.__id, fʹ.e.__id))),
                  [tv, ρʹ, jʹ, σv]: EvalResult<T> = eval_<T>(Env.concat(ρ1, ρ2), EnvId.concat(j1, j2), σ, σʹu)
            return __result(k, Trace.App.at(kʹ, tf, tu, __nonNull(tv.trace)), tv.val, ρʹ, jʹ, σv)
         } else
         if (f instanceof Value.PrimOp) {
            const [tu, , , σʹu]: EvalResult<PrimBody<T>> = eval_(ρ, j, f.σ, e.arg),
                  [v, σv]: PrimResult<T> = σʹu(tu.val, σ)
            return __result(k, Trace.PrimApp.at(kʹ, tf, tu), v, Env.empty(), EnvId.empty(), σv)
         } else {
            return assert(false, "Not a function.", f)
         }
      }
   }
   return assert(false, "Demand mismatch.")
}

}
