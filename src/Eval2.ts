import { Annotation, ann } from "./util/Annotated2"
import { __nonNull, absurd, as, assert, className, error } from "./util/Core"
import { Cons, List, Nil, cons, nil } from "./BaseTypes2"
import { ctrFor } from "./DataType2"
import { Env, emptyEnv, extendEnv } from "./Env2"
import { Expl } from "./ExplValue2"
import { Expr } from "./Expr2"
import { instantiate, uninstantiate } from "./Instantiate2"
import { Match, Plug, evalTrie, plug, unmatch } from "./Match2"
import { UnaryOp, BinaryOp, binaryOps, unaryOps } from "./Primitive2"
import { DataValue, Id, Num, Str, Value, _, make } from "./Value2"
import { Versioned, VersionedC, at, copyAt, joinα, numʹ, setα, setExpl, strʹ } from "./Versioned2"

import Trie = Expr.Trie

type Def = Expr.Def
type Expl = Expl.Expl
type RecDef = Expr.RecDef
type Tag = "t" | "v" // TODO: expess in terms of keyof ExplVal?

export class EvalId<T extends Tag> extends Id {
   e: Expr | Versioned<Str> = _ // str case is for binding occurrences of variables
   tag: T = _
}

function evalId<T extends Tag> (e: Expr | Versioned<Str>, tag: T): EvalId<T> {
   return make(EvalId, e, tag) as EvalId<T>
}

export type ValId = EvalId<"v">
export type ExplId = EvalId<"t">

export module Eval {

export class Closure extends VersionedC(DataValue)<"Closure"> {
   ρ: Env = _                 // ρ not closing for σ; need to extend with the bindings in δ
   δ: List<RecDef> = _
   σ: Trie<Expr> = _
}

function closure (k: Id, ρ: Env, δ: List<RecDef>, σ: Trie<Expr>): Closure {
   return at(k, Closure, ρ, δ, σ)
}
   
// Environments are snoc-lists, so this (inconsequentially) reverses declaration order.
// TODO: associate explanations to the created closures.
function closeDefs (δ_0: List<RecDef>, ρ: Env, δ: List<RecDef>): Env {
   if (Cons.is(δ)) {
      const def: RecDef = δ.head,
            k: ValId = evalId(def.x, "v")
      return extendEnv(closeDefs(δ_0, ρ, δ.tail), def.x, setα(def.x.__α, closure(k, ρ, δ_0, def.σ)))
   } else
   if (Nil.is(δ)) {
      return emptyEnv()
   } else {
      return absurd()
   }
}

// ρ is a collection of one or more closures. Most of the required joins have already been computed.
function uncloseDefs (ρ: Env): void {
   const f̅: List<Closure> = ρ.entries().map((v: Versioned<Value>) => as(v, Closure))
   if (Cons.is(f̅)) {
      let δ: List<RecDef> = f̅.head.δ,
          f̅ʹ: List<Closure> = f̅
      for (; Cons.is(f̅ʹ) && Cons.is(δ); f̅ʹ = f̅ʹ.tail, δ = δ.tail) {
         joinα(f̅ʹ.head.__α, δ.head.x)
      }
   } else
   if (Nil.is(f̅)) {
   } else {
      return absurd()
   }
}

// TODO: associate explanations to values created in the let and primitive cases.
function def̅Env (ρ: Env, def̅: List<Def>, ρ_ext: Env): [List<Expl.Def>, Env] {
   if (Cons.is(def̅)) {
      const def: Def = def̅.head
      if (def instanceof Expr.Let) {
         const k: ValId = evalId(def.x, "v"),
               v: Versioned<Value> = eval_(ρ.concat(ρ_ext), instantiate(ρ_ext, def.e)),
               vʹ: Versioned<Value> = setα(ann.meet(v.__α, def.x.__α), copyAt(k, v)),
               [def̅ₜ, ρ_extʹ]: [List<Expl.Def>, Env] = def̅Env(ρ, def̅.tail, extendEnv(ρ_ext, def.x, vʹ))
         return [cons(Expl.let_(def.x, v, vʹ), def̅ₜ), ρ_extʹ]
      } else
      if (def instanceof Expr.Prim) {
         // first-class primitives currently happen to be unary
         if (unaryOps.has(def.x.val)) {
            const k: ValId = evalId(def.x, "v"),
                  op: UnaryOp = unaryOps.get(def.x.val)!,
                  opʹ: Versioned<UnaryOp> = setα(def.x.__α, copyAt(k, op)),
                  [def̅ₜ, ρ_extʹ]: [List<Expl.Def>, Env] = def̅Env(ρ, def̅.tail, extendEnv(ρ_ext, def.x, opʹ))
            return [cons(Expl.prim(def.x, op, opʹ), def̅ₜ), ρ_extʹ]
         } else {
            return error(`No implementation found for primitive "${def.x.val}".`)
         }
      } else
      if (def instanceof Expr.LetRec) {
         const ρᵟ: Env = closeDefs(def.δ, ρ.concat(ρ_ext), def.δ),
               [def̅ₜ, ρ_extʹ]: [List<Expl.Def>, Env] = def̅Env(ρ, def̅.tail, ρ_ext.concat(ρᵟ))
         return [cons(Expl.letRec(ρᵟ), def̅ₜ), ρ_extʹ]
      } else {
         return absurd()
      }
   } else
   if (Nil.is(def̅)) {
      return [nil(), ρ_ext]
   } else {
      return absurd()
   }
}

function undef̅Env (def̅: List<Expl.Def>): void {
   if (Cons.is(def̅)) {
      const def: Expl.Def = def̅.head
      if (def instanceof Expl.Let) {
         undef̅Env(def̅.tail)
         joinα(def.vʹ.__α, def.v)
         joinα(def.vʹ.__α, def.x)
         uninstantiate(uneval(def.v))
      } else
      if (def instanceof Expl.Prim) {
         undef̅Env(def̅.tail)
         joinα(def.vʹ.__α, def.x)
      } else
      if (def instanceof Expl.LetRec) {
         undef̅Env(def̅.tail)
         uncloseDefs(def.ρᵟ)
      } else {
         return absurd()
      }
   } else
   if (Nil.is(def̅)) {
   } else {
      return absurd()
   }
}

export function eval_ (ρ: Env, e: Expr): Versioned<Value> {
   const kₜ: ExplId = evalId(e, "t"),
         kᵥ: ValId = evalId(e, "v")
   if (e instanceof Expr.ConstNum) {
      return setExpl(Expl.empty(kₜ), setα(e.__α, numʹ(kᵥ, e.val.val)))
   } else
   if (e instanceof Expr.ConstStr) {
      return setExpl(Expl.empty(kₜ), setα(e.__α, strʹ(kᵥ, e.val.val)))
   } else
   if (e instanceof Expr.Fun) {
      return setExpl(Expl.empty(kₜ), setα(e.__α, closure(kᵥ, ρ, nil(), e.σ)))
   } else
   if (e instanceof Expr.Constr) {
      let v̅: Value[] = e.args.toArray().map((e: Expr) => eval_(ρ, e))
      return setExpl(Expl.empty(kₜ), setα(e.__α, at(kᵥ, ctrFor(e.ctr).C, ...v̅)))
   } else
   if (e instanceof Expr.Var) {
      if (ρ.has(e.x)) { 
         const v: Versioned<Value> = ρ.get(e.x)!
         return setExpl(Expl.var_(kₜ, e.x, v), setα(ann.meet(v.__α, e.__α), copyAt(kᵥ, v)))
      } else {
         return error(`Variable "${e.x.val}" not found.`)
      }
   } else
   if (e instanceof Expr.App) {
      const f: Versioned<Value> = eval_(ρ, e.func),
            u: Versioned<Value> = eval_(ρ, e.arg)
      if (f instanceof Closure) {
         const [ρʹ, {ξ, κ: eʹ}, α]: [Env, Plug<Expr>, Annotation] = evalTrie(f.σ).__apply(u),
               ρ_δ: Env = closeDefs(f.δ, f.ρ, f.δ),
               ρᶠ: Env = ρ_δ.concat(ρʹ),
               v: Versioned<Value> = eval_(f.ρ.concat(ρᶠ), instantiate(ρᶠ, eʹ))
         return setExpl(Expl.app(kₜ, f, u, ρ_δ, plug(ξ, v)), setα(ann.meet(f.__α, α, v.__α, e.__α), copyAt(kᵥ, v)))
      } else 
      if (f instanceof UnaryOp) {
         if (u instanceof Num || u instanceof Str) {
            return setExpl(Expl.unaryApp(kₜ, f, u), setα(ann.meet(f.__α, u.__α, e.__α), f.op(u)(kᵥ)))
         } else {
            return error(`Applying "${f.name}" to non-primitive value.`, u)
         }
      } else {
         return error(`Cannot apply ${className(f)}`)
      }
   } else
   // Binary operators are (currently) "syntax", rather than first-class.
   if (e instanceof Expr.BinaryApp) {
      if (binaryOps.has(e.opName.val)) {
         const op: BinaryOp = binaryOps.get(e.opName.val)!, // TODO: add annotations to opName
               [v1, v2]: [Versioned<Value>, Versioned<Value>] = [eval_(ρ, e.e1), eval_(ρ, e.e2)]
         if ((v1 instanceof Num || v1 instanceof Str) && (v2 instanceof Num || v2 instanceof Str)) {
               return setExpl(Expl.binaryApp(kₜ, v1, e.opName, v2), setα(ann.meet(v1.__α, v2.__α, e.__α), op.op(v1, v2)(kᵥ)))
         } else {
            return error(`Applying "${e.opName}" to non-primitive value.`, v1, v2)
         }
      } else {
         return error(`Binary primitive "${e.opName.val}" not found.`)
      }
   } else
   if (e instanceof Expr.Defs) {
      const [def̅ₜ, ρʹ]: [List<Expl.Def>, Env] = def̅Env(ρ, e.def̅, emptyEnv()),
            v: Versioned<Value> = eval_(ρ.concat(ρʹ), instantiate(ρʹ, e.e))
      return setExpl(Expl.defs(kₜ, def̅ₜ, v), setα(ann.meet(v.__α, e.__α), copyAt(kᵥ, v)))
   } else
   if (e instanceof Expr.MatchAs) {
      const u: Versioned<Value> = eval_(ρ, e.e),
            [ρʹ, {ξ, κ: eʹ}, α]: [Env, Plug<Expr>, Annotation] = evalTrie(e.σ).__apply(u),
            v: Versioned<Value> = eval_(ρ.concat(ρʹ), instantiate(ρʹ, eʹ))
      return setExpl(Expl.matchAs(kₜ, u, plug(ξ, v)), setα(ann.meet(α, v.__α, e.__α), copyAt(kᵥ, v)))
   } else {
      return absurd(`Unimplemented expression form: ${className(e)}.`)
   }
}

// Avoid excessive joins via a merging implementation; requires all annotations to have been cleared first.
export function uneval (v: Versioned<Value>): Expr {
   const k: ValId = v.__id as ValId,
         e: Expr = k.e as Expr,
         t: Expl = v.__expl
   if (t instanceof Expl.Empty) {
      if (v instanceof Num) {
         return joinα(v.__α, e)
      } else
      if (v instanceof Str) {
         return joinα(v.__α, e)
      } else
      if (v instanceof Closure) {
         assert(Nil.is(v.δ))
         return joinα(v.__α, e)
      } else 
      if (v instanceof DataValue) {
         // reverse order but shouldn't matter in absence of side-effects:
         v.fieldValues().map(uneval)
         return joinα(v.__α, e)
      } else {
         return absurd()
      }
   } else
   if (t instanceof Expl.Var) {
      joinα(v.__α, t.v)
      return joinα(v.__α, e)
   } else
   if (t instanceof Expl.App) {
      assert(t.f instanceof Closure)
      const {ξ, κ: v} = t.ξv
      joinα(v.__α, v)
      unmatch(plug(ξ as Match<Expr>, uninstantiate(uneval(v))), v.__α)
      uncloseDefs(t.ρᵟ)
      joinα(v.__α, t.f)
      uneval(t.f)
      uneval(t.u)
      return joinα(v.__α, e)
   } else
   if (t instanceof Expl.UnaryApp) {
      joinα(v.__α, t.f)
      joinα(v.__α, t.v)
      uneval(t.f)
      uneval(t.v)
      return joinα(v.__α, e)
   } else
   if (t instanceof Expl.BinaryApp) {
      assert(binaryOps.has(t.opName.val))
      joinα(v.__α, t.v1)
      joinα(v.__α, t.v2)
      uneval(t.v1)
      uneval(t.v2)
      return joinα(v.__α, e)
   } else
   if (t instanceof Expl.Defs) {
      joinα(v.__α, t.v)
      uninstantiate(uneval(t.v))
      undef̅Env(t.def̅)
      return joinα(v.__α, e)
   } else
   if (t instanceof Expl.MatchAs) {
      const {ξ, κ: v} = t.ξv
      joinα(v.__α, v)
      unmatch(plug(ξ as Match<Expr>, uninstantiate(uneval(v))), v.__α)
      uneval(t.u)
      return joinα(v.__α, e)
   } else {
      return absurd()
   }
}

}
