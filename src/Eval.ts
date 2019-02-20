import { __nonNull, absurd } from "./util/Core"
import { PersistentObject, Versioned, at, make, asVersioned } from "./util/Persistent"
import { Cons, List, Nil } from "./BaseTypes"
import { Bot, Env, EnvEntries, EnvEntry, ExtendEnv } from "./Env"
import { Expr } from "./Expr"
import { get, has } from "./FiniteMap"
import { BinaryOp, PrimResult, binaryOps } from "./Primitive"
import { Traced, Value, Value̊ } from "./Traced"

import App = Traced.App
import BotKont = Traced.BotKont
import Empty = Traced.Empty
import Kont = Traced.Kont
import Let = Traced.Let
import LetRec = Traced.LetRec
import MatchAs = Traced.MatchAs
import PrimApp = Traced.PrimApp
import Trie = Traced.Trie
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
class EvalKey<K extends Expr.Kont<K>> implements PersistentObject {
   j: EnvEntries
   e: Expr
   σ: Expr.Trie<K>

   constructor_ (j: EnvEntries, e: Expr, σ: Expr.Trie<K>) {
      this.j = j
      this.e = e
      this.σ = σ
   }

   static make<K extends Expr.Kont<K>> (j: EnvEntries, e: Expr, σ: Expr.Trie<K>): EvalKey<K> {
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
export function closeDefs (δ_0: List<Expr.RecDef>, ρ: Env, δ: List<Expr.RecDef>): Env {
   if (Cons.is(δ)) {
      return ExtendEnv.make(closeDefs(δ_0, ρ, δ.tail), δ.head.x.str, EnvEntry.make(ρ, δ_0, δ.head.e))
   } else
   if (Nil.is(δ)) {
      return ρ
   } else {
      return absurd()
   }
}

// Parser ensures constructor patterns agree with constructor signatures.
function evalArgs<K extends Expr.Kont<K>> (ρ: Env, Π: Expr.Args<K>, es: List<Expr>): Results<K> {
   if (Cons.is(es)) {
      let σ: Expr.Trie<Expr.Args<K>>
      if (Expr.Args.Next.is(Π)) {
         σ = Π.σ
      } else
      if (Expr.Args.Top.is(Π)) {
         σ = Expr.Trie.Top.make(Expr.Args.Top.make(Π.κ))
      } else
      if (Expr.Args.Bot.is(Π)) {
         σ = Expr.Trie.Bot.make()
      } else {
         return absurd()
      }
      const {tv, ρ: ρʹ, κ: Πʹ}: Result<Expr.Args<K>> = eval_(ρ, es.head, σ),
            // propagate bot:
            {tvs, ρ: ρʺ, κ}: Results<K> = evalArgs(ρ, Πʹ instanceof Expr.BotKont ? Expr.Args.Bot.make() : Πʹ, es.tail)
      return Results.make(Cons.make(tv, tvs), Env.concat(ρʹ, ρʺ), κ)
   } else
   if (Nil.is(es)) {
      if (Expr.Args.End.is(Π) || Expr.Args.Top.is(Π)) {
         return Results.make(Nil.make(), Env.empty(), Π.κ)
      } else
      if (Expr.Args.Bot.is(Π)) {
         return Results.make(Nil.make(), Env.empty(), BotKont.make() as K)
      } else {
         return absurd()
      }
   } else {
      return absurd()
   }
}

export function eval_<K extends Expr.Kont<K>> (ρ: Env, e: Expr, σ: Expr.Trie<K>): Result<K> {
   const e_: Versioned<Expr> = asVersioned(e),
         k: TraceId<Expr> = e_.__id as TraceId<Expr>,
         kᵥ: ValId = EvalId.make(k.j, k.e, "val"),
         out: EvalKey<K> = EvalKey.make(k.j, k.e, σ)
   if (Expr.Trie.Bot.is(σ)) { 
      return Result.at(out, e, Bot.make(), BotKont.make() as K)
   } else
   if (Expr.Trie.Var.is(σ)) {
      const entry: EnvEntry = EnvEntry.make(ρ, Nil.make(), e)
      return Result.at(out, e, Env.singleton(σ.x.str, entry), σ.κ)
   } else
   if (e instanceof Expr.Bot) {
       // top demands "match" bottom; see issue #74
      return Result.at(out, e, Bot.make(), σ instanceof Trie.Top ? σ.κ : BotKont.make() as K) 
   } else
   if (e instanceof Expr.Constr) {
      let Π: Expr.Args<K>
      if (Expr.Trie.Constr.is(σ) && has(σ.cases, e.ctr.str)) {
         Π = get(σ.cases, e.ctr.str)!
      } else 
      if (Expr.Trie.Top.is(σ)) {
         Π = Expr.Args.Top.make(σ.κ)
      } else {
         // leave out Top case for now
         return absurd("Demand mismatch.", e, σ)
      }
      const {tvs: args, ρ: ρʹ, κ}: Results<K> = evalArgs(ρ, Π, e.args)
      return Result.at(out, Traced.make(Empty.at(k), Value.Constr.at(kᵥ, e.ctr, args)), ρʹ, κ)
   } else
   if (e instanceof Expr.ConstInt && Expr.Trie.ConstInt.is(σ)) {
      return Result.at(out, Traced.make(Empty.at(k), Value.ConstInt.at(kᵥ, e.val)), Env.empty(), σ.κ)
   } else
   if (e instanceof Expr.ConstStr && Expr.Trie.ConstStr.is(σ)) {
      return Result.at(out, Traced.make(Empty.at(k), Value.ConstStr.at(kᵥ, e.val)), Env.empty(), σ.κ)
   } else
   if (e instanceof Expr.Fun && Expr.Trie.Fun.is(σ)) {
      return Result.at(out, Traced.make(Empty.at(k), Value.Closure2.at(kᵥ, ρ, e.σ)), Env.empty(), σ.κ)
   } else
   if (e instanceof Expr.PrimOp && Expr.Trie.Fun.is(σ)) {
      return Result.at(out, Traced.make(Empty.at(k), Value.PrimOp.at(kᵥ, e.op)), Env.empty(), σ.κ)
   } else
   if (e instanceof Expr.Var) {
      const x: string = e.x.str
      if (ρ.has(x)) { 
         const {ρ: ρʹ, δ, e: eʹ}: EnvEntry = ρ.get(x)!,
               {tv, ρ: ρʺ, κ}: Result<K> = eval_(closeDefs(δ, ρʹ, δ), eʹ, σ)
         return Result.at(out, Traced.make(Var.at(k, e.x, __nonNull(tv.t)), tv.v), ρʺ, κ)
      } else {
         return absurd("Variable not found.", x)
      }
   } else
   if (e instanceof Expr.App) {
      const {tv: tf}: Result<VoidKont> = eval_(ρ, e.func, Expr.Trie.Fun.make(Expr.VoidKont.make())),
            f: Value̊ = tf.v
      if (f instanceof Value.Closure2) {
         const {tv: tu, ρ: ρʹ, κ: eʹ}: Result<Expr> = eval_(ρ, e.arg, f.σ),
               {tv, ρ: ρʺ, κ}: Result<K> = eval_(Env.concat(f.ρ, ρʹ), eʹ, σ)
         return Result.at(out, Traced.make(App.at(k, tf, tu, __nonNull(tv.t)), tv.v), ρʺ, κ)
      } else
      // Primitives with identifiers as names are unary and first-class.
      if (f instanceof Value.PrimOp) {
         const {tv: tu}: Result<VoidKont> = eval_(ρ, e.arg, f.op.σ),
               [v, κ]: PrimResult<K> = f.op.b.invoke(tu.v!, σ)(kᵥ)
         return Result.at(out, Traced.make(App.at(k, tf, tu, null), v), Env.empty(), κ)
      } else {
         return absurd()
      }
   } else
   if (e instanceof Expr.Let) {
      const {tv: tu, ρ: ρʹ, κ: eʹ}: Result<Expr> = eval_(ρ, e.e, e.σ),
            {tv, ρ: ρʺ, κ}: Result<K> = eval_(Env.concat(ρ, ρʹ), eʹ, σ)
      return Result.at(out, Traced.make(Let.at(k, tu, Expr.Trie.Var.make(e.σ.x, __nonNull(tv.t))), tv.v), ρʺ, κ)
   } else
   if (e instanceof Expr.LetRec) {
      const ρʹ: Env = closeDefs(e.δ, ρ, e.δ),
            {tv, ρ: ρʺ, κ}: Result<K> = eval_(ρʹ, e.e, σ)
      return Result.at(out, Traced.make(LetRec.at(k, e.δ, tv), tv.v), ρʺ, κ)
   } else
   if (e instanceof Expr.MatchAs) {
      const {tv: tu, ρ: ρʹ, κ: eʹ}: Result<Expr> = eval_(ρ, e.e, e.σ),
            {tv, ρ: ρʺ, κ}: Result<K> = eval_(Env.concat(ρ, ρʹ), eʹ, σ)
      return Result.at(out, Traced.make(MatchAs.at(k, tu, e.σ, __nonNull(tv.t)), tv.v), ρʺ, κ)
   } else
   // Operators (currently all binary) are "syntax", rather than names.
   if (e instanceof Expr.PrimApp) {
      if (binaryOps.has(e.opName.str)) {
         const op: BinaryOp = binaryOps.get(e.opName.str)!,
               {tv: tv1}: Result<VoidKont> = eval_(ρ, e.e1, op.σ1),
               {tv: tv2}: Result<VoidKont> = eval_(ρ, e.e2, op.σ2),
               [v, κ]: PrimResult<K> = op.b.invoke(tv1.v!, tv2.v!, σ)(kᵥ)
         return Result.at(out, Traced.make(PrimApp.at(k, tv1, e.opName, tv2), v), Env.empty(), κ)
      } else {
         return absurd("Operator name not found.", e.opName)
      }
   } else {
      return absurd("Unimplemented expression form.", e)
   }
}

}
