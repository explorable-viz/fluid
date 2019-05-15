import { __nonNull, absurd, className, error } from "./util/Core"
import { Cons, List, Nil, nil } from "./BaseTypes2"
import { ctrFor } from "./DataType2"
import { Env, emptyEnv, extendEnv } from "./Env2"
import { Expl, explValue } from "./ExplValue2"
import { Expr } from "./Expr2"
import { Closure, closure } from "./Func2"
import { instantiate } from "./Instantiate2"
import { evalTrie } from "./Match2"
import { UnaryOp, BinaryOp, binaryOps } from "./Primitive2"
import { Id, Value, _, make } from "./Value2"
import { at, copyAt, numʹ, strʹ } from "./Versioned2"

type Tag = "t" | "v" // TODO: expess in terms of keyof ExplVal?

export class Tagged<T extends Tag> extends Id {
   e: Expr | Expr.RecDef = _
   tag: T = _
}

export function tagged<T extends Tag> (e: Expr | Expr.RecDef, tag: T): Tagged<T> {
   return make(Tagged, e, tag) as Tagged<T>
}

export type ValId = Tagged<"v">
export type ExplId = Tagged<"t">

export module Eval {

// Environments are snoc-lists, so this (inconsequentially) reverses declaration order.
export function closeDefs (δ_0: List<Expr.RecDef>, ρ: Env, δ: List<Expr.RecDef>): Env {
   if (Cons.is(δ)) {
      const def: Expr.RecDef = δ.head,
            kᵥ: ValId = tagged(def, "v")
      return extendEnv(closeDefs(δ_0, ρ, δ.tail), def.x, closure(kᵥ, ρ, δ_0, def.σ))
   } else
   if (Nil.is(δ)) {
      return emptyEnv()
   } else {
      return absurd()
   }
}

export function eval_ (ρ: Env, e: Expr): Value {
   const kₜ: ExplId = tagged(e, "t"),
         kᵥ: ValId = tagged(e, "v")
   if (e instanceof Expr.ConstNum) {
      return explValue(Expl.empty(kₜ), numʹ(kᵥ, e.val.val))
   } else
   if (e instanceof Expr.ConstStr) {
      return explValue(Expl.empty(kₜ), strʹ(kᵥ, e.val.val))
   } else
   if (e instanceof Expr.Fun) {
      return explValue(Expl.empty(kₜ), closure(kᵥ, ρ, nil(), e.σ))
   } else
   if (e instanceof Expr.Constr) {
      let v̅: Value[] = e.args.toArray().map((e: Expr) => eval_(ρ, e))
      return explValue(Expl.empty(kₜ), at(kᵥ, ctrFor(e.ctr).C, ...v̅))
   } else 
   if (e instanceof Expr.Var) {
      if (ρ.has(e.x)) { 
         return explValue(Expl.var_(kₜ, e.x), copyAt(kᵥ, ρ.get(e.x)!))
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
         return explValue(Expl.app(kₜ, f, u), copyAt(kᵥ, v))
      } else 
      if (f instanceof UnaryOp) {
         return explValue(Expl.unaryApp(kₜ, f, u), f.op(u)(kᵥ))
      } else {
         return error(`Cannot apply ${className(f)}`)
      }
   } else
   // Operators (currently all binary) are "syntax", rather than names.
   if (e instanceof Expr.BinaryApp) {
      if (binaryOps.has(e.opName.val)) {
         const op: BinaryOp = binaryOps.get(e.opName.val)!, // opName lacks annotations
               [v1, v2]: [Value, Value] = [eval_(ρ, e.e1), eval_(ρ, e.e2)]
         return explValue(Expl.binaryApp(kₜ, v1, e.opName, v2), op.op(v1, v2)(kᵥ))
      } else {
         return error(`Operator ${e.opName.val} not found.`)
      }
   } else
   if (e instanceof Expr.Let) {
      const u: Value = eval_(ρ, e.e),
            [ρʹ, eʹ]: [Env, Expr] = evalTrie<Expr>(e.σ).__apply(u),
            v: Value = eval_(ρ.concat(ρʹ), instantiate(ρʹ, eʹ))
      return explValue(Expl.let_(kₜ, u), copyAt(kᵥ, v))
   } else
   if (e instanceof Expr.LetRec) {
      const ρʹ: Env = closeDefs(e.δ, ρ, e.δ),
            v: Value = eval_(ρ.concat(ρʹ), instantiate(ρʹ, e.e))
      return explValue(Expl.letRec(kₜ, e.δ), copyAt(kᵥ, v))
   } else
   if (e instanceof Expr.MatchAs) {
      const u: Value = eval_(ρ, e.e),
            [ρʹ, eʹ]: [Env, Expr] = evalTrie(e.σ).__apply(u),
            v: Value = eval_(ρ.concat(ρʹ), instantiate(ρʹ, eʹ))
      return explValue(Expl.matchAs(kₜ, u), copyAt(kᵥ, v))
   } else {
      return absurd(`Unimplemented expression form: ${className(e)}.`)
   }
}

}
