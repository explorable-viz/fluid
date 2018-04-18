import { __nonNull, absurd, assert, as, make } from "./util/Core"
import { Cons, List, Nil } from "./BaseTypes"
import { ctrToDataType } from "./DataType"
import { Env, EnvEntries, EnvEntry, ExtendEnv } from "./Env"
import { get, has } from "./FiniteMap"
import { instantiate } from "./Instantiate"
import { BinaryOp, PrimResult, binaryOps } from "./Primitive"
import { Expr, Trace, Traced, Trie, Value } from "./Syntax"
import { Persistent, PersistentObject } from "./Runtime";

export module Eval {

export class Evaluand extends PersistentObject {
   j: EnvEntries
   e: Expr

   static make (j: EnvEntries, e: Expr): Evaluand {
      const this_: Evaluand = make(Evaluand, j, e)
      this_.j = j
      this_.e = e
      return this_
   }
}

export type Result<T> = [Traced, Env, T]                    // tv, ρ, σv
type Results = [List<Traced>, Env, PersistentObject | null] // tvs, ρ, σv

function closeDefs (δ_0: Expr.RecDefs, ρ: Env, δ: Expr.RecDefs): Env {
   if (δ_0 instanceof Expr.EmptyRecDefs) {
      return ρ
   } else 
   if (δ_0 instanceof Expr.ExtendRecDefs) {
      return ExtendEnv.make(closeDefs(δ_0.δ, ρ, δ), δ_0.def.x.str, EnvEntry.make(ρ, δ, δ_0.def.def))
   } else {
      return absurd()
   }
}

// Not capturing the polymorphic type of the nested trie κ (which has a depth of n >= 0).
function evalSeq (ρ: Env, κ: PersistentObject | null, es: List<Expr>): Results {
   if (Cons.is(es)) {
      const σ: Trie<PersistentObject> = as(κ as Trie<PersistentObject>, Trie.Trie),
            [tv, ρʹ, κʹ]: Result<Persistent> = eval_(ρ, es.head, σ),
            [tvs, ρʺ, κʺ]: Results = evalSeq(ρ, κʹ, es.tail)
      return [Cons.make(tv, tvs), Env.concat(ρʹ, ρʺ), κʺ]
   } else
   if (Nil.is(es)) {
      return [Nil.make(), Env.empty(), κ]
   } else {
      return absurd()
   }
}

export function eval_<T extends PersistentObject | null> (ρ: Env, e: Expr, σ: Trie<T>): Result<T> {
   return evalT(ρ, instantiate(ρ)(e), σ)
}

// Output trace and value are unknown (null) iff σ is empty (i.e. a variable trie).
export function evalT<T extends PersistentObject | null> (ρ: Env, e: Traced, σ: Trie<T>): Result<T> {
   const k: Evaluand = e.__id
   if (Trie.Var.is(σ)) {
      const entry: EnvEntry = EnvEntry.make(ρ, Expr.EmptyRecDefs.make(), e)
      return [Traced.at(k, null, null), Env.singleton(σ.x.str, entry), σ.body]
   } else {
      const t: Trace | null = e.trace
      if (t instanceof Trace.Empty) {

      } else
      if (t instanceof Trace.Var) {
         const x: string = t.ident.str
         if (ρ.has(x)) {
            const {ρ: ρʹ, δ, e: eʹ}: EnvEntry = ρ.get(x)!,
                  [tv, ρʺ, σv]: Result<T> = eval_(closeDefs(δ, ρʹ, δ), eʹ, σ),
                  t: Trace = Trace.Var.at(k, e.ident, __nonNull(tv.trace))
            return [Traced.at(k, t, tv.val), ρʺ, σv]
         } else {
            return assert(false, "Variable not found.", x)
         }
      } else
      if (t instanceof Trace.App) {
         const [tf, ,]: Result<null> = eval_(ρ, t.func, Trie.Fun.make(null)),
               f: Value | null = tf.val
         if (f instanceof Value.Closure) {
            const [tu, ρʹ, σʹu]: Result<Expr> = eval_(ρ, t.arg, f.σ),
                  [tv, ρʺ, σv]: Result<T> = eval_<T>(Env.concat(f.ρ, ρʹ), σʹu, σ)
            return [Traced.at(k, Trace.App.at(k, tf, tu, __nonNull(tv.trace)), tv.val), ρʺ, σv]
         } else
         // Primitives with identifiers as names are unary and first-class.
         if (f instanceof Value.PrimOp) {
            const [tu, ,]: Result<null> = eval_(ρ, t.arg, f.op.σ),
                  [v, κ]: PrimResult<T> = f.op.b.invoke(tu.val!, σ)(k)
            return [Traced.at(k, Trace.App.at(k, tf, tu, null), v), Env.empty(), κ]
         } else {
            return assert(false, "Not a function.", f)
         }
      } else
      if (t instanceof Trace.Let) {
         const [tu, ρʹ, σu]: Result<Expr> = eval_(ρ, t.e, t.σ),
               [tv, ρʺ, κ]: Result<T> = eval_<T>(Env.concat(ρ, ρʹ), σu, σ)
         return [Traced.at(k, Trace.Let.at(k, tu, t.σ, __nonNull(tv.trace)), tv.val), ρʺ, κ]
      } else
      if (t instanceof Trace.LetRec) {
         const ρʹ: Env = closeDefs(t.δ, ρ, t.δ),
               [tv, ρʺ, σv]: Result<T> = eval_<T>(ρʹ, t.t, σ)
         return [Traced.at(k, Trace.LetRec.at(k, t.δ, __nonNull(tv.trace)), tv.val), ρʺ, σv]
      } else
      if (t instanceof Trace.MatchAs) {
         const [tu, ρʹ, σu]: Result<Expr> = eval_(ρ, t.tu, t.σ),
               [tv, ρʺ, κ]: Result<T> = eval_<T>(Env.concat(ρ, ρʹ), σu, σ)
         return [Traced.at(k, Trace.MatchAs.at(k, tu, t.σ, __nonNull(tv.trace)), tv.val), ρʺ, κ]
      } else
      // Operators (currently all binary) are "syntax", rather than names.
      if (t instanceof Trace.PrimApp) {
         if (binaryOps.has(t.opName.str)) {
            const op: BinaryOp = binaryOps.get(t.opName.str)!,
                  [tv1, ,]: Result<null> = eval_(ρ, t.tv1, op.σ1),
                  [tv2, ,]: Result<null> = eval_(ρ, t.tv2, op.σ2),
                  [v, σv]: PrimResult<T> = op.b.invoke(tv1.val!, tv2.val!, σ)(k)
            return [Traced.at(k, Trace.PrimApp.at(k, tv1, t.opName, tv2), v), Env.empty(), σv]
         } else {
            return assert(false, "Operator name not found.", t.opName)
         }
      }

      if (e instanceof Expr.Constr && Trie.Constr.is(σ) && has(σ.cases, e.ctr.str)) {
         const ctr: string = e.ctr.str
         assert(ctrToDataType.has(ctr), "No such constructor.", e)
         assert(ctrToDataType.get(ctr)!.ctrs.get(ctr)!.length === e.args.length, "Arity mismatch.", e)
         const σʹ: PersistentObject | null = get(σ.cases, e.ctr.str)!,
               [tvs, ρʹ, κ]: Results = evalSeq(ρ, σʹ, e.args)
         // have to cast κ without type information on constructor
         return [Traced.at(k, Trace.Empty.at(k), Value.Constr.at(k, e.ctr, tvs)), ρʹ, κ as T]
      } else
      if (e instanceof Expr.ConstInt && Trie.ConstInt.is(σ)) {
         return [Traced.at(k, Trace.Empty.at(k), Value.ConstInt.at(k, e.val)), Env.empty(), σ.body]
      } else
      if (e instanceof Expr.ConstStr && Trie.ConstStr.is(σ)) {
         return [Traced.at(k, Trace.Empty.at(k), Value.ConstStr.at(k, e.val)), Env.empty(), σ.body]
      } else
      if (e instanceof Expr.Fun && Trie.Fun.is(σ)) {
         return [Traced.at(k, Trace.Empty.at(k), Value.Closure.at(k, ρ, e.σ)), Env.empty(), σ.body]
      } else
      if (e instanceof Expr.PrimOp && Trie.Fun.is(σ)) {
         return [Traced.at(k, Trace.Empty.at(k), Value.PrimOp.at(k, e.op)), Env.empty(), σ.body]
      }
   }
   return assert(false, "Demand mismatch.", e, σ)
}

}
