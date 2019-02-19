import { __nonNull, absurd, assert } from "./util/Core"
import { PersistentObject, Versioned, at, make, asVersioned } from "./util/Persistent"
import { Cons, List, Nil } from "./BaseTypes"
import { Bot, Env, EnvEntries, EnvEntry, ExtendEnv } from "./Env"
import { Expr } from "./Expr"
import { get, has } from "./FiniteMap"
import { instantiate } from "./Instantiate"
import { BinaryOp, PrimResult, binaryOps } from "./Primitive"
import { Trace, Traced, Value, Value̊ } from "./Traced"

import App = Traced.App
import Args = Traced.Args
import BotKont = Traced.BotKont
import Empty = Traced.Empty
import Kont = Traced.Kont
import Let = Traced.Let
import LetRec = Traced.LetRec
import MatchAs = Traced.MatchAs
import PrimApp = Traced.PrimApp
import Trie = Traced.Trie
import RecDef = Traced.RecDef
import Var = Traced.Var
import VoidKont = Traced.VoidKont

export class EvalId<E extends Expr | Expr.RecDef, T extends "val" | "trace"> {
   j: EnvEntries
   e: E
   tag: T

   constructor_ (j: EnvEntries, e: E, tag: T) {
      this.j = j
      this.e = e
      this.tag = tag
   }

   static make<E extends Expr | Expr.RecDef, T extends "val" | "trace"> (j: EnvEntries, e: E, tag: T): EvalId<E, T> {
      return make(EvalId, j, e, tag) as EvalId<E, T>
   }
}

export type ValId = EvalId<Expr, "val">
export type TraceId<E extends Expr | Expr.RecDef> = EvalId<E, "trace">

export module Eval {

// Note that an "eval id" is not a suitable memo-key for eval_: different demands will produce output environments of 
// different shapes. (For the same reason, eval_ is only monotone w.r.t. σ in the output environment if the ordering on
// tries implies equality of binding structure.) This effectively serves as an eval_ memo key in the meantime; probably
// want to subsume this into some memoisation infrastructure at some point.
class EvalKey<K extends Kont<K>> implements PersistentObject {
   j: EnvEntries
   e: Expr
   σ: Trie<K>

   constructor_ (j: EnvEntries, e: Expr, σ: Trie<K>) {
      this.j = j
      this.e = e
      this.σ = σ
   }

   static make<K extends Kont<K>> (j: EnvEntries, e: Expr, σ: Trie<K>): EvalKey<K> {
      return make(EvalKey, j, e, σ) as EvalKey<K>
   }
}
   
// Versioned so that we can access prior value of the environment when forward-slicing.
export class Result<K extends Kont<K>> implements PersistentObject {
   tv: Traced
   ρ: Env
   κ: K

   constructor_ (tv: Traced, ρ: Env, κ: K): void {
      this.tv = tv
      this.ρ = ρ
      this.κ = κ
   }

   static at<K extends Kont<K>> (α: PersistentObject, tv: Traced, ρ: Env, κ: K): Result<K> {
      if (κ instanceof Traced.Bot) {
         return absurd()
      }
      return at(α, Result, tv, ρ, κ) as Result<K>
   }
}

export class Results<K extends Kont<K>> implements PersistentObject {
   tvs: List<Traced>
   ρ: Env
   κ: K

   constructor_ (tvs: List<Traced>, ρ: Env, κ: K): void {
      this.tvs = tvs
      this.ρ = ρ
      this.κ = κ
   }

   static make<K extends Kont<K>> (tvs: List<Traced>, ρ: Env, κ: K): Results<K> {
      return make(Results, tvs, ρ, κ) as Results<K>
   }
}

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
function evalArgs<K extends Kont<K>> (ρ: Env, Π: Args<K>, es: List<Traced>): Results<K> {
   if (Cons.is(es)) {
      let σ: Trie<Args<K>>
      if (Args.Next.is(Π)) {
         σ = Π.σ
      } else
      if (Args.Top.is(Π)) {
         σ = Trie.Top.make(Args.Top.make(Π.κ))
      } else
      if (Args.Bot.is(Π)) {
         σ = Trie.Bot.make()
      } else {
         return absurd()
      }
      const {tv, ρ: ρʹ, κ: Πʹ}: Result<Args<K>> = eval__(ρ, es.head, σ),
            // propagate bot:
            {tvs, ρ: ρʺ, κ}: Results<K> = evalArgs(ρ, Πʹ instanceof BotKont ? Args.Bot.make() : Πʹ, es.tail)
      return Results.make(Cons.make(tv, tvs), Env.concat(ρʹ, ρʺ), κ)
   } else
   if (Nil.is(es)) {
      if (Args.End.is(Π) || Args.Top.is(Π)) {
         return Results.make(Nil.make(), Env.empty(), Π.κ)
      } else
      if (Args.Bot.is(Π)) {
         return Results.make(Nil.make(), Env.empty(), BotKont.make() as K)
      } else {
         return absurd()
      }
   } else {
      return absurd()
   }
}

// Preprocess with call to instantiate. TODO: why this approach rather than the one in the paper?
function eval__<K extends Kont<K>> (ρ: Env, e: Traced, σ: Trie<K>): Result<K> {
   const t: Versioned<Trace> = asVersioned(e.t),
         k: TraceId<Expr> = t!.__id as TraceId<Expr>
   return eval_(ρ, instantiate(ρ)(k.e), σ)
}

// Null means eval produced no information about v; the input traced value might be non-null.
// By the time we get here e should have been "instantiated" with respect to ρ.
export function eval_<K extends Kont<K>> (ρ: Env, e: Traced, σ: Trie<K>): Result<K> {
   const t: Versioned<Trace> = asVersioned(e.t),
         k: TraceId<Expr> = t.__id as TraceId<Expr>,
         kᵥ: ValId = EvalId.make(k.j, k.e, "val"),
         out: EvalKey<K> = EvalKey.make(k.j, k.e, σ)
   if (σ instanceof Trie.Bot) { // 'is' check confuses compiler
      return Result.at(out, e, Bot.make(), BotKont.make() as K)
   } else
   if (Trie.Var.is(σ)) {
      const entry: EnvEntry = EnvEntry.make(ρ, Nil.make(), e)
      return Result.at(out, e, Env.singleton(σ.x.str, entry), σ.κ)
   } else {
      if (t instanceof Traced.Bot) {
         // top demands "match" bottom; see issue #74
         return Result.at(out, e, Bot.make(), σ instanceof Trie.Top ? σ.κ : BotKont.make() as K) 
      } else
      if (t instanceof Empty) {
         const v: Versioned<Value> = __nonNull(asVersioned(e.v))
         assert(v.__id === kᵥ)
         if (v instanceof Value.Constr) {
            let Π: Args<K>
            if (Trie.Constr.is(σ) && has(σ.cases, v.ctr.str)) {
               Π = get(σ.cases, v.ctr.str)!
            } else
            if (Trie.Top.is(σ)) {
               Π = Args.Top.make(σ.κ)
            } else {
               return absurd("Demand mismatch.", e, σ)
            }
            const {tvs: args, ρ: ρʹ, κ}: Results<K> = evalArgs(ρ, Π, v.args)
            return Result.at(out, Traced.make(t, Value.Constr.at(kᵥ, v.ctr, args)), ρʹ, κ)
         } else
         if (v instanceof Value.ConstInt && (Trie.ConstInt.is(σ) || Trie.Top.is(σ))) {
            return Result.at(out, e, Env.empty(), σ.κ)
         } else
         if (v instanceof Value.ConstStr && (Trie.ConstStr.is(σ) || Trie.Top.is(σ))) {
            return Result.at(out, e, Env.empty(), σ.κ)
         } else
         if ((v instanceof Value.Closure || v instanceof Value.PrimOp) && (Trie.Fun.is(σ) || Trie.Top.is(σ))) {
            return Result.at(out, e, Env.empty(), σ.κ)
         } else {
            return absurd("Demand mismatch.", e, σ)
         }
      } else
      if (t instanceof Var) {
         const x: string = t.x.str
         if (ρ.has(x)) { 
            const {ρ: ρʹ, δ, e: eʹ}: EnvEntry = ρ.get(x)!,
                  {tv, ρ: ρʺ, κ}: Result<K> = eval__(closeDefs(δ, ρʹ, δ), eʹ, σ)
            return Result.at(out, Traced.make(Var.at(k, t.x, __nonNull(tv.t)), tv.v), ρʺ, κ)
         } else {
            return absurd("Variable not found.", x)
         }
      } else
      if (t instanceof App) {
         const {tv: tf}: Result<VoidKont> = eval__(ρ, t.func, Trie.Fun.make(VoidKont.make())),
               f: Value̊ = tf.v
         if (f instanceof Value.Closure) {
            const {tv: tu, ρ: ρʹ, κ: eʹ}: Result<Traced> = eval__(ρ, t.arg, f.σ)
            if (eʹ instanceof BotKont) {
               return Result.at(out, Traced.make(App.at(k, tf, tu, null), null), Bot.make(), BotKont.make() as K)
            } else {
               const {tv, ρ: ρʺ, κ}: Result<K> = eval__(Env.concat(f.ρ, ρʹ), eʹ, σ)
               return Result.at(out, Traced.make(App.at(k, tf, tu, __nonNull(tv.t)), tv.v), ρʺ, κ)
            }
         } else
         // Primitives with identifiers as names are unary and first-class.
         if (f instanceof Value.PrimOp) {
            const {tv: tu}: Result<VoidKont> = eval__(ρ, t.arg, f.op.σ),
                  [v, κ]: PrimResult<K> = f.op.b.invoke(tu.v!, σ)(kᵥ)
            return Result.at(out, Traced.make(App.at(k, tf, tu, null), v), Env.empty(), κ)
         } else {
            return absurd()
         }
      } else
      if (t instanceof Let) {
         const {tv: tu, ρ: ρʹ, κ: eʹ}: Result<Traced> = eval__(ρ, t.tu, t.σ),
               {tv, ρ: ρʺ, κ}: Result<K> = eval__(Env.concat(ρ, ρʹ), eʹ, σ)
         return Result.at(out, Traced.make(Let.at(k, tu, t.σ, __nonNull(tv.t)), tv.v), ρʺ, κ)
      } else
      if (t instanceof LetRec) {
         const ρʹ: Env = closeDefs(t.δ, ρ, t.δ),
               {tv, ρ: ρʺ, κ}: Result<K> = eval__(ρʹ, t.tv, σ)
         return Result.at(out, Traced.make(LetRec.at(k, t.δ, tv), tv.v), ρʺ, κ)
      } else
      if (t instanceof MatchAs) {
         const {tv: tu, ρ: ρʹ, κ: eʹ}: Result<Traced> = eval__(ρ, t.tu, t.σ),
               {tv, ρ: ρʺ, κ}: Result<K> = eval__(Env.concat(ρ, ρʹ), eʹ, σ)
         return Result.at(out, Traced.make(MatchAs.at(k, tu, t.σ, __nonNull(tv.t)), tv.v), ρʺ, κ)
      } else
      // Operators (currently all binary) are "syntax", rather than names.
      if (t instanceof PrimApp) {
         if (binaryOps.has(t.opName.str)) {
            const op: BinaryOp = binaryOps.get(t.opName.str)!,
                  {tv: tv1}: Result<VoidKont> = eval__(ρ, t.tv1, op.σ1),
                  {tv: tv2}: Result<VoidKont> = eval__(ρ, t.tv2, op.σ2),
                  [v, κ]: PrimResult<K> = op.b.invoke(tv1.v!, tv2.v!, σ)(kᵥ)
            return Result.at(out, Traced.make(PrimApp.at(k, tv1, t.opName, tv2), v), Env.empty(), κ)
         } else {
            return absurd("Operator name not found.", t.opName)
         }
      } else {
         return absurd("Unimplemented expression form.", t)
      }
   }
}

}
