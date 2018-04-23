import { __nonNull, absurd, assert, make } from "./util/Core"
import { Cons, List, Nil } from "./BaseTypes"
import { Env, EnvEntries, EnvEntry, ExtendEnv } from "./Env"
import { get, has } from "./FiniteMap"
import { instantiate } from "./Instantiate"
import { BinaryOp, PrimResult, binaryOps } from "./Primitive"
import { Expr, Trace, Traced, Trie, TrieBody, Value } from "./Syntax"
import { PersistentObject } from "./Runtime";

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

export type Result<T> = [Traced, Env, T] // tv, ρ, κ
type Results<T> = [List<Traced>, Env, T] // tvs, ρ, κ

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
function evalSeq<T extends PersistentObject | null> (ρ: Env, κ: TrieBody<T>, es: List<Traced>): Results<T> {
   if (Cons.is(es) && Trie.Trie.is(κ)) {
      const [tv, ρʹ, κʹ]: Result<TrieBody<T>> = eval_(ρ, es.head, κ),
            [tvs, ρʺ, κʺ]: Results<T> = evalSeq(ρ, κʹ, es.tail)
      return [Cons.make(tv, tvs), Env.concat(ρʹ, ρʺ), κʺ]
   } else
   if (Nil.is(es)) {
      // want to assert that κ is dynamically a T; not the same as not being a Trie.
      return [Nil.make(), Env.empty(), κ as T]
   } else {
      return absurd()
   }
}

// Probably want to memoise instantiate.
export function eval_<T extends PersistentObject | null> (ρ: Env, tv: Traced, σ: Trie<T>): Result<T> {
   return evalT(ρ, instantiate(ρ)(tv.t!.__id.e), σ)
}

// Value is unknown (null) iff σ is a variable trie.
export function evalT<T extends PersistentObject | null> (ρ: Env, tv: Traced, σ: Trie<T>): Result<T> {
   const t: Trace | null = tv.t,
         k: Runtime<Expr> = t.__id
   if (Trie.Var.is(σ)) {
      const entry: EnvEntry = EnvEntry.make(ρ, Nil.make(), tv)
      return [Traced.make(t, null), Env.singleton(σ.x.str, entry), σ.body]
   } else {
      if (t instanceof Trace.Empty) {
         const v: Value = __nonNull(tv.v)
         assert(v.__id === k && t.__id === k)
         if (v instanceof Value.Constr && Trie.Constr.is(σ) && has(σ.cases, v.ctr.str)) {
            const [, ρʹ, κ]: Results<T> = evalSeq(ρ, get(σ.cases, v.ctr.str)!, v.args)
            return [Traced.make(t, v), ρʹ, κ]
         } else
         if (v instanceof Value.ConstInt && Trie.ConstInt.is(σ)) {
            return [Traced.make(t, v), Env.empty(), σ.body]
         } else
         if (v instanceof Value.ConstStr && Trie.ConstStr.is(σ)) {
            return [Traced.make(t, v), Env.empty(), σ.body]
         } else
         if (v instanceof Value.Closure && Trie.Fun.is(σ)) {
            return [Traced.make(t, v), Env.empty(), σ.body]
         } else
         if (v instanceof Value.PrimOp && Trie.Fun.is(σ)) {
            return [Traced.make(t, v), Env.empty(), σ.body]
         } else {
            return assert(false, "Demand mismatch.", tv, σ)
         }
      } else
      if (t instanceof Trace.Var) {
         const x: string = t.x.str
         if (ρ.has(x)) {
            const {ρ: ρʹ, δ, e: eʹ}: EnvEntry = ρ.get(x)!,
                  [tv, ρʺ, σv]: Result<T> = eval_(closeDefs(δ, ρʹ, δ), eʹ, σ)
            return [Traced.make(Trace.Var.at(k, t.x, __nonNull(tv.t)), tv.v), ρʺ, σv]
         } else {
            return assert(false, "Variable not found.", x)
         }
      } else
      if (t instanceof Trace.App) {
         const [tf, ,]: Result<null> = eval_(ρ, t.func, Trie.Fun.make(null)),
               f: Value | null = tf.v
         if (f instanceof Value.Closure) {
            const [tu, ρʹ, eʹ]: Result<Traced> = eval_(ρ, t.arg, f.σ),
                  [tv, ρʺ, κ]: Result<T> = eval_<T>(Env.concat(f.ρ, ρʹ), eʹ, σ)
            return [Traced.make(Trace.App.at(k, tf, tu, __nonNull(tv.t)), tv.v), ρʺ, κ]
         } else
         // Primitives with identifiers as names are unary and first-class.
         if (f instanceof Value.PrimOp) {
            const [tu, ,]: Result<null> = eval_(ρ, t.arg, f.op.σ),
                  [v, κ]: PrimResult<T> = f.op.b.invoke(tu.v!, σ)(k)
            return [Traced.make(Trace.App.at(k, tf, tu, null), v), Env.empty(), κ]
         } else {
            return absurd()
         }
      } else
      if (t instanceof Trace.Let) {
         const [tu, ρʹ, eʹ]: Result<Traced> = eval_(ρ, t.tu, t.σ),
               [tv, ρʺ, κ]: Result<T> = eval_<T>(Env.concat(ρ, ρʹ), eʹ, σ)
         return [Traced.make(Trace.Let.at(k, tu, t.σ, __nonNull(tv.t)), tv.v), ρʺ, κ]
      } else
      if (t instanceof Trace.LetRec) {
         const ρʹ: Env = closeDefs(t.δ, ρ, t.δ),
               [tv, ρʺ, κ]: Result<T> = eval_<T>(ρʹ, t.tv, σ)
         return [Traced.make(Trace.LetRec.at(k, t.δ, tv), tv.v), ρʺ, κ]
      } else
      if (t instanceof Trace.MatchAs) {
         const [tu, ρʹ, σu]: Result<Traced> = eval_(ρ, t.tu, t.σ),
               [tv, ρʺ, κ]: Result<T> = eval_<T>(Env.concat(ρ, ρʹ), σu, σ)
         return [Traced.make(Trace.MatchAs.at(k, tu, t.σ, __nonNull(tv.t)), tv.v), ρʺ, κ]
      } else
      // Operators (currently all binary) are "syntax", rather than names.
      if (t instanceof Trace.PrimApp) {
         if (binaryOps.has(t.opName.str)) {
            const op: BinaryOp = binaryOps.get(t.opName.str)!,
                  [tv1, ,]: Result<null> = eval_(ρ, t.tv1, op.σ1),
                  [tv2, ,]: Result<null> = eval_(ρ, t.tv2, op.σ2),
                  [v, κ]: PrimResult<T> = op.b.invoke(tv1.v!, tv2.v!, σ)(k)
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
