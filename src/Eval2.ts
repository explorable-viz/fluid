import { ann } from "./util/Annotated2"
import { __nonNull, absurd, className, error } from "./util/Core"
import { Cons, List, Nil, nil } from "./BaseTypes2"
import { ctrFor } from "./DataType2"
import { Env, emptyEnv, extendEnv } from "./Env2"
import { Constr } from "./DataType2"
import { Expl } from "./ExplValue2"
import { Expr } from "./Expr2"
import { instantiate } from "./Instantiate2"
import { evalTrie } from "./Match2"
import { UnaryOp, BinaryOp, binaryOps } from "./Primitive2"
import { Id, Value, _, make } from "./Value2"
import { at, copyAt, copyα, getα, numʹ, setα, setExpl, strʹ } from "./Versioned2"

import Trie = Expr.Trie

type Tag = "t" | "v" // TODO: expess in terms of keyof ExplVal?

export class EvalId<T extends Tag> extends Id {
   e: Expr | Expr.RecDef = _
   tag: T = _
}

export function evalId<T extends Tag> (e: Expr | Expr.RecDef, tag: T): EvalId<T> {
   return make(EvalId, e, tag) as EvalId<T>
}

export type ValId = EvalId<"v">
export type ExplId = EvalId<"t">

export module Eval {

export class Closure extends Constr<"Closure"> {
   ρ: Env = _                 // ρ not closing for σ; need to extend with the bindings in δ
   δ: List<Expr.RecDef> = _
   σ: Trie<Expr> = _
}

export function closure (k: Id, ρ: Env, δ: List<Expr.RecDef>, σ: Trie<Expr>): Closure {
   return at(k, Closure, ρ, δ, σ)
}
   
// Environments are snoc-lists, so this (inconsequentially) reverses declaration order.
export function closeDefs (δ_0: List<Expr.RecDef>, ρ: Env, δ: List<Expr.RecDef>): Env {
   if (Cons.is(δ)) {
      const def: Expr.RecDef = δ.head,
            kᵥ: ValId = evalId(def, "v")
      return extendEnv(closeDefs(δ_0, ρ, δ.tail), def.x, copyα(def, closure(kᵥ, ρ, δ_0, def.σ)))
   } else
   if (Nil.is(δ)) {
      return emptyEnv()
   } else {
      return absurd()
   }
}

export function defsEnv (ρ: Env, defs: List<Expr.Def>): Env {
   if (Cons.is(defs)) {
      const def: Expr.Def = defs.head
      if (def instanceof Expr.Let2) {
         return ρ.concat(Env.singleton(def.x, eval_(ρ, def.e)))
      } else
      if (def instanceof Expr.LetRec2) {
         return ρ.concat(closeDefs(def.δ, ρ, def.δ))
      } else {
         return absurd()
      }
   } else
   if (Nil.is(defs)) {
      return ρ
   } else {
      return absurd()
   }
}

export function eval_ (ρ: Env, e: Expr): Value {
   const kₜ: ExplId = evalId(e, "t"),
         kᵥ: ValId = evalId(e, "v")
   if (e instanceof Expr.ConstNum) {
      return setExpl(Expl.empty(kₜ), copyα(e, numʹ(kᵥ, e.val.val)))
   } else
   if (e instanceof Expr.ConstStr) {
      return setExpl(Expl.empty(kₜ), copyα(e, strʹ(kᵥ, e.val.val)))
   } else
   if (e instanceof Expr.Fun) {
      return setExpl(Expl.empty(kₜ), copyα(e, closure(kᵥ, ρ, nil(), e.σ)))
   } else
   if (e instanceof Expr.Constr) {
      let v̅: Value[] = e.args.toArray().map((e: Expr) => eval_(ρ, e))
      return setExpl(Expl.empty(kₜ), copyα(e, at(kᵥ, ctrFor(e.ctr).C, ...v̅)))
   } else
   if (e instanceof Expr.Var) {
      if (ρ.has(e.x)) { 
         const v: Value = ρ.get(e.x)!
         return setExpl(Expl.var_(kₜ, e.x), setα(ann.meet(getα(v), getα(e)), copyAt(kᵥ, v)))
      } else {
         return error(`Variable '${e.x.val}' not found.`)
      }
   } else
   if (e instanceof Expr.App) {
      const f: Value = eval_(ρ, e.func),
            u: Value = eval_(ρ, e.arg)
      if (f instanceof Closure) {
         const [ρʹ, eʹ]: [Env, Expr] = evalTrie(f.σ).__apply(u),
               ρᶠ: Env = closeDefs(f.δ, f.ρ, f.δ).concat(ρʹ),
               v: Value = eval_(f.ρ.concat(ρᶠ), instantiate(ρᶠ, eʹ))
         return setExpl(Expl.app(kₜ, f, u), setα(ann.meet(getα(f), getα(v), getα(e)), copyAt(kᵥ, v)))
      } else 
      if (f instanceof UnaryOp) {
         return setExpl(Expl.unaryApp(kₜ, f, u), setα(ann.meet(getα(f), getα(u), getα(e)), f.op(u)(kᵥ)))
      } else {
         return error(`Cannot apply ${className(f)}`)
      }
   } else
   // Binary operators are (currently) "syntax", rather than first-class.
   if (e instanceof Expr.BinaryApp) {
      if (binaryOps.has(e.opName.val)) {
         const op: BinaryOp = binaryOps.get(e.opName.val)!, // opName lacks annotations
               [v1, v2]: [Value, Value] = [eval_(ρ, e.e1), eval_(ρ, e.e2)]
         return setExpl(Expl.binaryApp(kₜ, v1, e.opName, v2), setα(ann.meet(getα(v1), getα(v2), getα(e)), op.op(v1, v2)(kᵥ)))
      } else {
         return error(`Operator ${e.opName.val} not found.`)
      }
   } else
   if (e instanceof Expr.Defs) {
      const v: Value = eval_(defsEnv(ρ, e.defs), e.e)
      return setExpl(Expl.defs(kₜ), setα(ann.meet(getα(v), getα(e)), copyAt(kᵥ, v)))
   } else
   if (e instanceof Expr.Let) {
      const u: Value = eval_(ρ, e.e),
            [ρʹ, eʹ]: [Env, Expr] = evalTrie<Expr>(e.σ).__apply(u),
            v: Value = eval_(ρ.concat(ρʹ), instantiate(ρʹ, eʹ))
      return setExpl(Expl.let_(kₜ, u), setα(ann.meet(getα(v), getα(e)), copyAt(kᵥ, v)))
   } else
   if (e instanceof Expr.LetRec) {
      const ρʹ: Env = closeDefs(e.δ, ρ, e.δ),
            v: Value = eval_(ρ.concat(ρʹ), instantiate(ρʹ, e.e))
      return setExpl(Expl.letRec(kₜ, e.δ), setα(ann.meet(getα(v), getα(e)), copyAt(kᵥ, v)))
   } else
   if (e instanceof Expr.MatchAs) {
      const u: Value = eval_(ρ, e.e),
            [ρʹ, eʹ]: [Env, Expr] = evalTrie(e.σ).__apply(u),
            v: Value = eval_(ρ.concat(ρʹ), instantiate(ρʹ, eʹ))
      return setExpl(Expl.matchAs(kₜ, u), setα(ann.meet(getα(v), getα(e)), copyAt(kᵥ, v)))
   } else {
      return absurd(`Unimplemented expression form: ${className(e)}.`)
   }
}



}
