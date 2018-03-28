import { zip } from "./util/Array"
import { __nonNull, assert, as } from "./util/Core"
import { extend, union } from "./util/Map"
import { __def, key, keyP } from "./Memo"
import { PrimBody } from "./Primitive"
import { Env, EnvEntry, Expr, Trace, Traced, Trie, Value } from "./Syntax"

export module Eval {

export type EvalResult<T> = [Traced, Env, T]    // tv, ρ, σv
type EvalResults = [Traced[], Env, Object] // tvs, ρ, σv

function __result<T> (α: Addr, t: Trace.Trace, v: Value.Value | null, ρ: Env, κ: T): EvalResult<T> {
   return [Traced.at(α, t, v), ρ, κ]
}

// Don't think I capture the polymorphic type of the nested trie κ (which has a depth of n >= 0).
function evalSeq (ρ: Env, κ: Object, es: Expr.Expr[]): EvalResults {
   if (es.length === 0) {
      return [[], new Map, κ]
   } else {
      const σ: Trie.Trie<Object> = as(κ as Trie.Trie<Object>, Trie.Trie),
            [tv, ρʹ, κʹ]: EvalResult<Object> = eval_(ρ, σ, es[0]),
            [tvs, ρʺ, κʺ]: EvalResults = evalSeq(ρ, κʹ, es.slice(1))
      return [[tv].concat(tvs), union([ρʹ, ρʺ]), κʺ]
   }
}

__def(eval_)
export function eval_<T> (ρ: Env, σ: Trie.Trie<T>, e: Expr.Expr): EvalResult<T> {
   const α: Addr = key(eval_, arguments)
   assert(e !== undefined, "Missing constructor argument?")
   if (σ instanceof Trie.Var) {
      const entries: [string, EnvEntry][] = [[σ.x.str, {ρ, δ: [], e}]]
      return __result(α, Trace.Empty.at(α), null, new Map(entries), σ.body)
   } else {
      if (e instanceof Expr.Constr && σ instanceof Trie.Constr) {
         const σʹ: Object = σ.cases.get(e.ctr.str),
               β: Addr = keyP(α, "val"),
               [tvs, ρʹ, κ]: EvalResults = evalSeq(ρ, σʹ, e.args)
         // have to cast κ without type information on constructor
         return __result(α, Trace.Empty.at(α), Value.Constr.at(β, e.ctr, tvs), ρʹ, κ as T)
      } else
      if (e instanceof Expr.ConstInt && σ instanceof Trie.Prim) {
         return __result(α, Trace.Empty.at(α), Value.ConstInt.at(keyP(α, "val"), e.val), new Map, σ.body)
      } else
      if (e instanceof Expr.ConstStr && σ instanceof Trie.Prim) {
         return __result(α, Trace.Empty.at(α), Value.ConstStr.at(keyP(α, "val"), e.val), new Map, σ.body)

      } else
      if (e instanceof Expr.Fun && σ instanceof Trie.Fun) {
         const v: Value.Closure = Value.Closure.at(keyP(α, "val"), ρ, [], e)
         return __result(α, Trace.Empty.at(keyP(α, "trace")), v, new Map, σ.body)
      } else
      if (e instanceof Expr.PrimOp && σ instanceof Trie.Fun) {
         return __result(α, Trace.Empty.at(keyP(α, "trace")), e.op, new Map, σ.body)
      } else
      if (e instanceof Expr.OpName || e instanceof Expr.Var) {
         const x: string = e instanceof Expr.OpName ? e.opName.str : e.ident.str
         if (!ρ.has(x)) {
            return assert(false, "Name not found.", x)
         } else {
            const cls: EnvEntry = __nonNull(ρ.get(x)),
                  [tv, ρʺ, σv]: EvalResult<T> = eval_<T>(cls.ρ, σ, cls.e),
                  t: Trace.Trace = e instanceof Expr.OpName 
                     ? Trace.OpName.at(α, e.opName, tv.trace)
                     : Trace.Var.at(α, e.ident, tv.trace)
            return __result(α, t, tv.val, ρʺ, σv)
         }
      } else
      if (e instanceof Expr.Let) {
         const [tu, ρʹ, σu]: EvalResult<Expr.Expr> = eval_(ρ, e.σ, e.e),
               [tv, ρʺ, κ]: EvalResult<T> = eval_<T>(union([ρ, ρʹ]), σ, σu)
         return __result(α, Trace.Let.at(keyP(α, "trace"), tu, tv.trace), tv.val, ρʺ, κ)
      } else 
      // See 0.3.4 release notes for semantics.
      if (e instanceof Expr.LetRec) {
         const fs: EnvEntry[] = e.δ.map(def => new EnvEntry(ρ, e.δ, def.func)),
               ρʹ: Env = extend(ρ, zip(e.δ.map(def => def.name.str), fs)),
               [tv, ρʺ, σv]: EvalResult<T> = eval_<T>(ρʹ, σ, e.e)
         return __result(α, Trace.LetRec.at(keyP(α, "trace"), e.δ, tv.trace), tv.val, ρʺ, σv)
      } else
      if (e instanceof Expr.MatchAs) {
         const [tu, ρʹ, σu]: EvalResult<Expr.Expr> = eval_(ρ, e.σ, e.e),
               [tv, ρʺ, κ]: EvalResult<T> = eval_<T>(union([ρ, ρʹ]), σ, σu)
         return __result(α, Trace.Match.at(keyP(α, "trace"), tu, tv.trace), tv.val, ρʺ, κ)
      } else
      if (e instanceof Expr.App) { 
         const β: Addr = keyP(α, "trace"),
               [tf, ,]: EvalResult<null> = eval_<null>(ρ, Trie.Fun.at(keyP(α, "1"), null), e.func),
               f: Value.Value | null = tf.val
         if (f instanceof Value.Closure) {
            const [tu, ρ2, σʹu]: EvalResult<Expr.Expr> = eval_(ρ, f.func.σ, e.arg),
                  fs: EnvEntry[] = f.δ.map(def => new EnvEntry(f.ρ, f.δ, def.func)),
                  ρ1: Env = extend(f.ρ, zip(f.δ.map(def => def.name.str), fs)),
                  [tv, ρʹ, σv]: EvalResult<T> = eval_<T>(union([ρ1, ρ2]), σ, σʹu)
            return __result(α, Trace.App.at(β, tf, tu, tv.trace), tv.val, ρʹ, σv)
         } else
         if (f instanceof Value.PrimOp) {
            const [tu, , σʹu]: EvalResult<PrimBody<T>> = eval_<PrimBody<T>>(ρ, f.σ, e.arg),
                  [tv, ρʹ, σv]: EvalResult<T> = σʹu(tu.val, σ)
            return __result(α, Trace.PrimApp.at(β, tf, tv), f._apply(tv.val, σ), ρʹ, σv)
         } else {
            return assert(false, "Not a function.", f)
         }
      }
   }
   return assert(false, "Demand mismatch.")
}

}
