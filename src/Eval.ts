import { __check, __nonNull, absurd, as, assert, make } from "./util/Core"
import { Cons, List, Nil } from "./BaseTypes"
import { Env, EnvEntries, EnvEntry, ExtendEnv } from "./Env"
import { Expr } from "./Expr"
import { get, has } from "./FiniteMap"
import { instantiate } from "./Instantiate"
import { match } from "./Match"
import { BinaryOp, PrimResult, binaryOps } from "./Primitive"
import { PersistentObject } from "./Runtime";
import { Kont, Trace, Traced, Trie, Value } from "./Traced"

export class Runtime<E extends Expr | Expr.RecDef> extends PersistentObject {
   j: EnvEntries
   e: E

   static make<E extends Expr | Expr.RecDef> (j: EnvEntries, e: E): Runtime<E> {
      const this_: Runtime<E> = make<Runtime<E>>(Runtime, j, e)
      this_.j = j
      this_.e = e
      return this_
   }
}

export module Eval {

export type Result = [Traced, Env, Kont]    // tv, ρ, κ
type Results = [List<Traced>, Env, Kont]    // tvs, ρ, κ

// Environments are snoc-lists, so this reverses declaration order, but semantically it's irrelevant.
export function closeDefs (δ_0: List<Trace.RecDef>, ρ: Env, δ: List<Trace.RecDef>): Env {
   if (Cons.is(δ)) {
      return ExtendEnv.make(closeDefs(δ_0, ρ, δ.tail), δ.head.x.str, EnvEntry.make(ρ, δ_0, δ.head.tv))
   } else
   if (Nil.is(δ)) {
      return ρ
   } else {
      return absurd()
   }
}

// Parser ensures constructor patterns agree with constructor signatures.
function evalArgs (ρ: Env, Π: Trie.Args, es: List<Traced>): Results {
   if (Cons.is(es) && Π instanceof Trie.Next) {
      const [tv, ρʹ, Πʹ]: Result = eval_(ρ, es.head, Π.σ),
            [tvs, ρʺ, κ]: Results = evalArgs(ρ, as(Πʹ, Trie.Args), es.tail)
      return [Cons.make(tv, tvs), Env.concat(ρʹ, ρʺ), κ]
   } else
   if (Nil.is(es) && Π instanceof Trie.End) {
      return [Nil.make(), Env.empty(), Π.κ]
   } else {
      return absurd()
   }
}

// Probably want to memoise instantiate.
export function eval_ (ρ: Env, tv: Traced, σ: Trie): Result {
   return __check(
      evalT(ρ, instantiate(ρ)(tv.t!.__id.e), σ), 
      ([tv, ,]) => {
         match(σ, tv.v) // Temporarily, check this doesn't crash.
         return (tv.v === null) === (σ instanceof Trie.Var)
      }
   )
}

// Null means eval produced no information about v; the input traced value might be non-null.
function evalT (ρ: Env, tv: Traced, σ: Trie): Result {
   const t: Trace | null = tv.t,
         k: Runtime<Expr> = t.__id
   if (σ instanceof Trie.Var) {
      const entry: EnvEntry = EnvEntry.make(ρ, Nil.make(), tv)
      return [Traced.make(t, null), Env.singleton(σ.x.str, entry), σ.κ]
   } else {
      if (t instanceof Trace.Empty) {
         const v: Value = __nonNull(tv.v)
         assert(v.__id === k && t.__id === k)
         if (v instanceof Value.Constr && σ instanceof Trie.Constr && has(σ.cases, v.ctr.str)) {
            const [args, ρʹ, κ]: Results = evalArgs(ρ, get(σ.cases, v.ctr.str)!, v.args)
            return [Traced.make(t, Value.Constr.at(k, v.ctr, args)), ρʹ, κ]
         } else
         if (v instanceof Value.ConstInt && σ instanceof Trie.ConstInt) {
            return [Traced.make(t, v), Env.empty(), σ.κ]
         } else
         if (v instanceof Value.ConstStr && σ instanceof Trie.ConstStr) {
            return [Traced.make(t, v), Env.empty(), σ.κ]
         } else
         if ((v instanceof Value.Closure || v instanceof Value.PrimOp) && σ instanceof Trie.Fun) {
            return [Traced.make(t, v), Env.empty(), σ.κ]
         } else {
            return assert(false, "Demand mismatch.", tv, σ)
         }
      } else
      if (t instanceof Trace.Var) {
         const x: string = t.x.str
         if (ρ.has(x)) {
            const {ρ: ρʹ, δ, e: eʹ}: EnvEntry = ρ.get(x)!,
                  [tv, ρʺ, κ]: Result = eval_(closeDefs(δ, ρʹ, δ), eʹ, σ)
            return [Traced.make(Trace.Var.at(k, t.x, __nonNull(tv.t)), tv.v), ρʺ, κ]
         } else {
            return assert(false, "Variable not found.", x)
         }
      } else
      if (t instanceof Trace.App) {
         const [tf, ,]: Result = eval_(ρ, t.func, Trie.Fun.make(null)),
               f: Value | null = tf.v
         if (f instanceof Value.Closure) {
            const [tu, ρʹ, eʹ]: Result = eval_(ρ, t.arg, f.σ),
                  [tv, ρʺ, κ]: Result = eval_(Env.concat(f.ρ, ρʹ), as(eʹ, Traced), σ)
            return [Traced.make(Trace.App.at(k, tf, tu, __nonNull(tv.t)), tv.v), ρʺ, κ]
         } else
         // Primitives with identifiers as names are unary and first-class.
         if (f instanceof Value.PrimOp) {
            const [tu, ,]: Result = eval_(ρ, t.arg, f.op.σ),
                  [v, κ]: PrimResult = f.op.b.invoke(tu.v!, σ)(k)
            return [Traced.make(Trace.App.at(k, tf, tu, null), v), Env.empty(), κ]
         } else {
            return absurd()
         }
      } else
      if (t instanceof Trace.Let) {
         const [tu, ρʹ, eʹ]: Result = eval_(ρ, t.tu, t.σ),
               [tv, ρʺ, κ]: Result = eval_(Env.concat(ρ, ρʹ), as(eʹ, Traced), σ)
         return [Traced.make(Trace.Let.at(k, tu, t.σ, __nonNull(tv.t)), tv.v), ρʺ, κ]
      } else
      if (t instanceof Trace.LetRec) {
         const ρʹ: Env = closeDefs(t.δ, ρ, t.δ),
               [tv, ρʺ, κ]: Result = eval_(ρʹ, t.tv, σ)
         return [Traced.make(Trace.LetRec.at(k, t.δ, tv), tv.v), ρʺ, κ]
      } else
      if (t instanceof Trace.MatchAs) {
         const [tu, ρʹ, eʹ]: Result = eval_(ρ, t.tu, t.σ),
               [tv, ρʺ, κ]: Result = eval_(Env.concat(ρ, ρʹ), as(eʹ, Traced), σ)
         return [Traced.make(Trace.MatchAs.at(k, tu, t.σ, __nonNull(tv.t)), tv.v), ρʺ, κ]
      } else
      // Operators (currently all binary) are "syntax", rather than names.
      if (t instanceof Trace.PrimApp) {
         if (binaryOps.has(t.opName.str)) {
            const op: BinaryOp = binaryOps.get(t.opName.str)!,
                  [tv1, ,]: Result = eval_(ρ, t.tv1, op.σ1),
                  [tv2, ,]: Result = eval_(ρ, t.tv2, op.σ2),
                  [v, κ]: PrimResult = op.b.invoke(tv1.v!, tv2.v!, σ)(k)
            return [Traced.make(Trace.PrimApp.at(k, tv1, t.opName, tv2), v), Env.empty(), κ]
         } else {
            return assert(false, "Operator name not found.", t.opName)
         }
      } else {
         return assert(false, "Demand mismatch.", tv, σ)
      }
   }
}

}
