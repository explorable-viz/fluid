import { __check, __nonNull, absurd, assert, make } from "./util/Core"
import { Cons, List, Nil } from "./BaseTypes"
import { Env, EnvEntries, EnvEntry, ExtendEnv } from "./Env"
import { Expr } from "./Expr"
import { get, has } from "./FiniteMap"
import { instantiate } from "./Instantiate"
import { BinaryOp, PrimResult, binaryOps } from "./Primitive"
import { PersistentObject } from "./Runtime";
import { Trace, Traced, Value } from "./Traced"

import App = Traced.App
import Args = Traced.Args
import Empty = Traced.Empty
import Let = Traced.Let
import LetRec = Traced.LetRec
import MatchAs = Traced.MatchAs
import PrimApp = Traced.PrimApp
import Trie = Traced.Trie
import RecDef = Traced.RecDef
import Var = Traced.Var

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

export type Result<K> = [Traced, Env, K]    // tv, ρ, κ
type Results<K> = [List<Traced>, Env, K]    // tvs, ρ, κ

// Environments are snoc-lists, so this reverses declaration order, but semantically it's irrelevant.
export function closeDefs (δ_0: List<RecDef>, ρ: Env, δ: List<RecDef>): Env {
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
function evalArgs<K> (ρ: Env, Π: Args<K>, es: List<Traced>): Results<K> {
   if (Cons.is(es)) {
      let σ
      if (Π instanceof Args.Next) {
         σ = Π.σ
      } else 
      if (Π instanceof Args.Top) {
         σ = Trie.Top.make(Π.κ)
      } else {
         return absurd()
      }
      const [tv, ρʹ, Πʹ]: Result<Args<K>> = eval_(ρ, es.head, σ),
            [tvs, ρʺ, κ]: Results<K> = evalArgs(ρ, Πʹ, es.tail)
      return [Cons.make(tv, tvs), Env.concat(ρʹ, ρʺ), κ]
   } else
   if (Nil.is(es) && (Π instanceof Args.End || Π instanceof Args.Top)) {
      return [Nil.make(), Env.empty(), Π.κ]
   } else {
      return absurd()
   }
}

// Probably want to memoise instantiate.
export function eval_<K> (ρ: Env, tv: Traced, σ: Trie<K>): Result<K> {
   return __check(
      evalT(ρ, instantiate(ρ)(tv.t!.__id.e), σ), 
      ([tv, ,]) => (tv.v === null) === (Trie.Var.is(σ))
   )
}

// Null means eval produced no information about v; the input traced value might be non-null.
function evalT<K> (ρ: Env, tv: Traced, σ: Trie<K>): Result<K> {
   const t: Trace | null = tv.t,
         k: Runtime<Expr> = t.__id
   if (Trie.Var.is(σ)) {
      const entry: EnvEntry = EnvEntry.make(ρ, Nil.make(), tv)
      return [Traced.make(t, null), Env.singleton(σ.x.str, entry), σ.κ]
   } else {
      if (t instanceof Empty) {
         const v: Value = __nonNull(tv.v)
         assert(v.__id === k && t.__id === k)
         if (v instanceof Value.Constr) {
            let σʹ
            if (Trie.Constr.is(σ) && has(σ.cases, v.ctr.str)) {
               σʹ = get(σ.cases, v.ctr.str)!
            } else
            if (Trie.Top.is(σ)) {
               σʹ = Args.Top.make(σ.κ)
            } else {
               return assert(false, "Demand mismatch.", tv, σ)
            }
            const [args, ρʹ, κ]: Results<K> = evalArgs(ρ, σʹ, v.args)
            return [Traced.make(t, Value.Constr.at(k, v.ctr, args)), ρʹ, κ]
         } else
         if (v instanceof Value.ConstInt && (Trie.ConstInt.is(σ) || Trie.Top.is(σ))) {
            return [Traced.make(t, v), Env.empty(), σ.κ]
         } else
         if (v instanceof Value.ConstStr && (Trie.ConstStr.is(σ) || Trie.Top.is(σ))) {
            return [Traced.make(t, v), Env.empty(), σ.κ]
         } else
         if ((v instanceof Value.Closure || v instanceof Value.PrimOp) && (Trie.Fun.is(σ) || Trie.Top.is(σ))) {
            return [Traced.make(t, v), Env.empty(), σ.κ]
         } else {
            return assert(false, "Demand mismatch.", tv, σ)
         }
      } else
      if (t instanceof Var) {
         const x: string = t.x.str
         if (ρ.has(x)) {
            const {ρ: ρʹ, δ, e: eʹ}: EnvEntry = ρ.get(x)!,
                  [tv, ρʺ, κ]: Result<K> = eval_(closeDefs(δ, ρʹ, δ), eʹ, σ)
            return [Traced.make(Var.at(k, t.x, __nonNull(tv.t)), tv.v), ρʺ, κ]
         } else {
            return assert(false, "Variable not found.", x)
         }
      } else
      if (t instanceof App) {
         const [tf, ,]: Result<null> = eval_(ρ, t.func, Trie.Fun.make(null)),
               f: Value | null = tf.v
         if (f instanceof Value.Closure) {
            const [tu, ρʹ, eʹ]: Result<Traced> = eval_(ρ, t.arg, f.σ),
                  [tv, ρʺ, κ]: Result<K> = eval_(Env.concat(f.ρ, ρʹ), eʹ, σ)
            return [Traced.make(App.at(k, tf, tu, __nonNull(tv.t)), tv.v), ρʺ, κ]
         } else
         // Primitives with identifiers as names are unary and first-class.
         if (f instanceof Value.PrimOp) {
            const [tu, ,]: Result<null> = eval_(ρ, t.arg, f.op.σ),
                  [v, κ]: PrimResult<K> = f.op.b.invoke(tu.v!, σ)(k)
            return [Traced.make(App.at(k, tf, tu, null), v), Env.empty(), κ]
         } else {
            return absurd()
         }
      } else
      if (t instanceof Let) {
         const [tu, ρʹ, eʹ]: Result<Traced> = eval_(ρ, t.tu, t.σ),
               [tv, ρʺ, κ]: Result<K> = eval_(Env.concat(ρ, ρʹ), eʹ, σ)
         return [Traced.make(Let.at(k, tu, t.σ, __nonNull(tv.t)), tv.v), ρʺ, κ]
      } else
      if (t instanceof LetRec) {
         const ρʹ: Env = closeDefs(t.δ, ρ, t.δ),
               [tv, ρʺ, κ]: Result<K> = eval_(ρʹ, t.tv, σ)
         return [Traced.make(LetRec.at(k, t.δ, tv), tv.v), ρʺ, κ]
      } else
      if (t instanceof MatchAs) {
         const [tu, ρʹ, eʹ]: Result<Traced> = eval_(ρ, t.tu, t.σ),
               [tv, ρʺ, κ]: Result<K> = eval_(Env.concat(ρ, ρʹ), eʹ, σ)
         return [Traced.make(MatchAs.at(k, tu, t.σ, __nonNull(tv.t)), tv.v), ρʺ, κ]
      } else
      // Operators (currently all binary) are "syntax", rather than names.
      if (t instanceof PrimApp) {
         if (binaryOps.has(t.opName.str)) {
            const op: BinaryOp = binaryOps.get(t.opName.str)!,
                  [tv1, ,]: Result<null> = eval_(ρ, t.tv1, op.σ1),
                  [tv2, ,]: Result<null> = eval_(ρ, t.tv2, op.σ2),
                  [v, κ]: PrimResult<K> = op.b.invoke(tv1.v!, tv2.v!, σ)(k)
            return [Traced.make(PrimApp.at(k, tv1, t.opName, tv2), v), Env.empty(), κ]
         } else {
            return assert(false, "Operator name not found.", t.opName)
         }
      } else {
         return assert(false, "Demand mismatch.", tv, σ)
      }
   }
}

}
