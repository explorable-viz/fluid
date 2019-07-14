import { ann } from "./util/Annotated"
import { zip } from "./util/Array"
import { __nonNull, absurd, as, assert, className, error } from "./util/Core"
import { Cons, List, Nil, cons, nil } from "./BaseTypes"
import { DataType, PrimType, ctrToDataType, initDataType, types } from "./DataType"
import { DataValue } from "./DataValue"
import { Env, emptyEnv, extendEnv } from "./Env"
import { Expl, ExplValue, explValue } from "./ExplValue"
import { Expr } from "./Expr"
import { get } from "./FiniteMap"
import { Elim, Match, evalTrie, match_bwd, match_fwd } from "./Match"
import { UnaryOp, BinaryOp, binaryOps, unaryOps } from "./Primitive"
import { Id, Num, Str, Value, _, make } from "./Value"
import { Annotated, AnnotatedC, ν, asAnnotated, at, copyAt, joinα, meetα, num, setα, str } from "./Versioned"

export enum Direction { Fwd, Bwd }
type Def = Expr.Def
type RecDef = Expr.RecDef

export module Eval {

// ρ plus bindings in δ are closing for f.
export class Closure extends AnnotatedC(DataValue)<"Closure"> {
   ρ: Env = _ 
   δ: List<RecDef> = _
   f: Elim<Expr> = _
}

function closure (k: Id, ρ: Env, δ: List<RecDef>, f: Elim<Expr>): Closure {
   return at(k, Closure, ρ, δ, f)
}

// Environments are snoc-lists, so this (inconsequentially) reverses declaration order.
function recDefs (δ_0: List<RecDef>, ρ: Env, δ: List<RecDef>): [List<Expl.RecDef>, Env] {
   if (Cons.is(δ)) {
      const def: RecDef = δ.head,
            [δₜ, ρ_ext]: [List<Expl.RecDef>, Env] = recDefs(δ_0, ρ, δ.tail),
            f: Closure = closure(ν(), ρ, δ_0, evalTrie(def.σ))
      return [cons(Expl.recDef(def.x, f), δₜ), extendEnv(ρ_ext, def.x, f)]
   } else
   if (Nil.is(δ)) {
      return [nil(), emptyEnv()]
   } else {
      return absurd()
   }
}

function recDefs_ (dir: Direction, δ: List<Expl.RecDef>): void {
   if (Cons.is(δ)) {
      zip(δ.head.f.δ.toArray(), δ.toArray()).map(([def, defₜ]: [RecDef, Expl.RecDef]): void => {
         if (dir === Direction.Fwd) {
            setα(def.x.__α, defₜ.f)
         } else {
            joinα(defₜ.f.__α, def.x)
         }
      })
   } else
   if (Nil.is(δ)) {
   } else {
      return absurd()
   }
}

// Here we mustn't invert definition order.
function defs (ρ: Env, def̅: List<Def>, ρ_ext: Env): [List<Expl.Def>, Env] {
   if (Cons.is(def̅)) {
      const def: Def = def̅.head
      if (def instanceof Expr.Let) {
         const tv: ExplValue = eval_(ρ.concat(ρ_ext), def.e),
               [def̅ₜ, ρ_extʹ]: [List<Expl.Def>, Env] = defs(ρ, def̅.tail, extendEnv(ρ_ext, def.x, tv.v))
         return [cons(Expl.let_(def.x, tv), def̅ₜ), ρ_extʹ]
      } else
      if (def instanceof Expr.Prim) {
         // first-class primitives currently happen to be unary
         if (unaryOps.has(def.x.val)) {
            const op: Annotated<UnaryOp> = unaryOps.get(def.x.val)!,
                  [def̅ₜ, ρ_extʹ]: [List<Expl.Def>, Env] = defs(ρ, def̅.tail, extendEnv(ρ_ext, def.x, op))
            return [cons(Expl.prim(def.x, op), def̅ₜ), ρ_extʹ]
         } else {
            return error(`No implementation found for primitive "${def.x.val}".`)
         }
      } else
      if (def instanceof Expr.LetRec) {
         const [δ, ρᵟ]: [List<Expl.RecDef>, Env] = recDefs(def.δ, ρ.concat(ρ_ext), def.δ),
               [def̅ₜ, ρ_extʹ]: [List<Expl.Def>, Env] = defs(ρ, def̅.tail, ρ_ext.concat(ρᵟ))
         return [cons(Expl.letRec(δ), def̅ₜ), ρ_extʹ]
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

function defs_fwd (def̅: List<Def>, def̅ₜ: List<Expl.Def>): void {
   zip(def̅.toArray(), def̅ₜ.toArray()).forEach(([def, defₜ]: [Def, Expl.Def]) => {
      if (defₜ instanceof Expl.Let) {         
         eval_fwd(as(def, Expr.Let).e, defₜ.tv)
         meetα(asAnnotated(defₜ.x).__α, asAnnotated(defₜ.tv.v))
      } else
      if (defₜ instanceof Expl.Prim) {
         setα(asAnnotated(defₜ.x).__α, asAnnotated(defₜ.op))
      } else
      if (defₜ instanceof Expl.LetRec) {
         recDefs_(Direction.Fwd, defₜ.δ)
      } else {
         absurd()
      }
   })
}

function defs_bwd (def̅: List<Def>, def̅ₜ: List<Expl.Def>): void {
   zip(def̅.toArray(), def̅ₜ.toArray()).reverse().forEach(([def, defₜ]: [Def, Expl.Def]) => {
      if (defₜ instanceof Expl.Let) {
         joinα(asAnnotated(defₜ.tv.v).__α, asAnnotated(defₜ.x))
         eval_bwd(as(def, Expr.Let).e, defₜ.tv)
      } else
      if (defₜ instanceof Expl.Prim) {
         joinα(asAnnotated(defₜ.op).__α, asAnnotated(defₜ.x))
      } else
      if (defₜ instanceof Expl.LetRec) {
         recDefs_(Direction.Bwd, defₜ.δ)
      } else {
         absurd()
      }
   })
}

export function eval_ (ρ: Env, e: Expr): ExplValue {
   if (e instanceof Expr.ConstNum) {
      return explValue(Expl.empty(), num(e.val.val))
   } else
   if (e instanceof Expr.ConstStr) {
      return explValue(Expl.empty(), str(e.val.val))
   } else
   if (e instanceof Expr.Fun) {
      return explValue(Expl.empty(), closure(ν(), ρ, nil(), evalTrie(e.σ)))
   } else
   if (e instanceof Expr.Constr) {
      let tv̅: ExplValue[] = e.args.toArray().map((e: Expr) => eval_(ρ, e)),
          c: string = e.ctr.val,
          d: DataType = __nonNull(ctrToDataType.get(c)),
          v: DataValue = at(ν(), d.ctrs.get(c)!.C, ...tv̅.map(({v}) => v))
      v.__expl = make(d.explC̅.get(c)!, ...tv̅.map(({t}) => t))
      return explValue(Expl.empty(), v)
   } else
   if (e instanceof Expr.Quote) {
      return explValue(Expl.quote(), copyAt(ν(), e.e))
   } else
   if (e instanceof Expr.Var) {
      if (ρ.has(e.x)) {
         const v: Value = ρ.get(e.x)!
         return explValue(Expl.var_(e.x, v), copyAt(ν(), v))
      } else {
         return error(`Variable "${e.x.val}" not found.`)
      }
   } else
   if (e instanceof Expr.App) {
      const [tf, tu]: [ExplValue, ExplValue] = [eval_(ρ, e.f), eval_(ρ, e.e)],
            [v, u]: [Value, Value] = [tf.v, tu.v]
      if (v instanceof Closure) {
         const [δ, ρᵟ]: [List<Expl.RecDef>, Env] = recDefs(v.δ, v.ρ, v.δ),
               [ρʹ, ξκ]: [Env, Match<Expr>] = v.f.apply(u),
               tv: ExplValue = eval_(v.ρ.concat(ρᵟ.concat(ρʹ)), ξκ.κ)
         return explValue(Expl.app(tf, tu, δ, ξκ, tv), copyAt(ν(), tv.v))
      } else 
      if (v instanceof UnaryOp) {
         if (u instanceof Num || u instanceof Str) {
            return explValue(Expl.unaryApp(tf, tu), v.op(u))
         } else {
            return error(`Applying "${v.name}" to non-primitive value.`, u)
         }
      } else {
         return error(`Cannot apply ${className(v)}`)
      }
   } else
   // Binary operators are (currently) "syntax", rather than first-class.
   if (e instanceof Expr.BinaryApp) {
      if (binaryOps.has(e.opName.val)) {
         const op: BinaryOp = binaryOps.get(e.opName.val)!,
               [tv1, tv2]: [ExplValue, ExplValue] = [eval_(ρ, e.e1), eval_(ρ, e.e2)],
               [v1, v2]: [Value, Value] = [tv1.v, tv2.v]
         if ((v1 instanceof Num || v1 instanceof Str) && (v2 instanceof Num || v2 instanceof Str)) {
               return explValue(Expl.binaryApp(tv1, e.opName, tv2), op.op(v1, v2))
         } else {
            return error(`Applying "${e.opName}" to non-primitive value.`, v1, v2)
         }
      } else {
         return error(`Binary primitive "${e.opName.val}" not found.`)
      }
   } else
   if (e instanceof Expr.Defs) {
      const [def̅ₜ, ρʹ]: [List<Expl.Def>, Env] = defs(ρ, e.def̅, emptyEnv()),
            tv: ExplValue = eval_(ρ.concat(ρʹ), e.e)
      return explValue(Expl.defs(def̅ₜ, tv), copyAt(ν(), tv.v))
   } else
   if (e instanceof Expr.MatchAs) {
      const tu: ExplValue = eval_(ρ, e.e),
            [ρʹ, ξκ]: [Env, Match<Expr>] = evalTrie(e.σ).apply(tu.v),
            tv: ExplValue = eval_(ρ.concat(ρʹ), ξκ.κ)
      return explValue(Expl.matchAs(tu, ξκ, tv), copyAt(ν(), tv.v))
   } else
   if (e instanceof Expr.Typematch) {
      const tu: ExplValue = eval_(ρ, e.e),
            d: DataType | PrimType = ctrToDataType.get(className(tu.v)) || types.get(className(tu.v))!,
            eʹ: Expr | undefined = get(e.cases, d.name)
      if (eʹ === undefined) {
         return error(`Typecase mismatch: no clause for ${className(tu.v)}.`)
      } else {
         const tv: ExplValue = eval_(ρ, eʹ)
         return explValue(Expl.typematch(tu, d.name, tv), copyAt(ν(), tv.v))
      }
   } else {
      return absurd(`Unimplemented expression form: ${className(e)}.`)
   }
}

export function eval_fwd (e: Expr, {t, v: vʹ}: ExplValue): void {
   const v: Annotated<Value> = asAnnotated(vʹ)
   if (t instanceof Expl.Empty) {
      if (v instanceof Num || v instanceof Str || v instanceof Closure) {
         setα(e.__α, v)
      } else
      if (v instanceof DataValue) {
         const eʹ: Expr.Constr = as(e, Expr.Constr)
         zip(v.fieldExplValues(), eʹ.args.toArray()).map(([[t, v], e]) => eval_fwd(e, explValue(t, v)))
         setα(e.__α, v)
      }
   } else
   if (t instanceof Expl.Quote) {
      setα(e.__α, v)
   } else
   if (t instanceof Expl.Var) {
      setα(ann.meet(e.__α, asAnnotated(t.v).__α), v)
   } else
   if (t instanceof Expl.App) {
      const eʹ: Expr.App = as(e, Expr.App)
      eval_fwd(eʹ.f, t.tf)
      eval_fwd(eʹ.e, t.tu)
      recDefs_(Direction.Fwd, t.δ)
      eval_fwd(t.ξ.κ, t.tv)
      setα(ann.meet(asAnnotated(t.tf.v).__α, match_fwd(t.ξ), e.__α, asAnnotated(t.tv.v).__α), v)
   } else
   if (t instanceof Expl.UnaryApp) {
      const eʹ: Expr.App = as(e, Expr.App)
      eval_fwd(eʹ.f, t.tf)
      eval_fwd(eʹ.e, t.tv)
      setα(ann.meet(asAnnotated(t.tf.v).__α, asAnnotated(t.tv.v).__α, e.__α), v)
   } else
   if (t instanceof Expl.BinaryApp) {
      const eʹ: Expr.BinaryApp = as(e, Expr.BinaryApp)
      eval_fwd(eʹ.e1, t.tv1)
      eval_fwd(eʹ.e2, t.tv2)
      setα(ann.meet(asAnnotated(t.tv1.v).__α, asAnnotated(t.tv2.v).__α, e.__α), v)
   } else
   if (t instanceof Expl.Defs) {
      const eʹ: Expr.Defs = as(e, Expr.Defs)
      defs_fwd(eʹ.def̅, t.def̅)
      eval_fwd(eʹ.e, t.tv)
      setα(ann.meet(e.__α, asAnnotated(t.tv.v).__α), v)
   } else
   if (t instanceof Expl.MatchAs) {
      const eʹ: Expr.MatchAs = as(e, Expr.MatchAs)
      eval_fwd(eʹ.e, t.tu)
      eval_fwd(t.ξ.κ, t.tv)
      setα(ann.meet(match_fwd(t.ξ), e.__α, asAnnotated(t.tv.v).__α), v)
   } else
   if (t instanceof Expl.Typematch) {
      const eʹ: Expr.Typematch = as(e, Expr.Typematch)
      eval_fwd(eʹ.e, t.tu)
      eval_fwd(get(eʹ.cases, t.d)!, t.tv)
      setα(ann.meet(e.__α, asAnnotated(t.tv.v).__α), v)
   } else {
      absurd()
   }
}

// Avoid excessive joins via a merging implementation; requires all annotations to have been cleared first.
export function eval_bwd (e: Expr, {t, v: vʹ}: ExplValue): Expr {
   const v: Annotated<Value> = asAnnotated(vʹ)
   if (t instanceof Expl.Empty) {
      if (v instanceof Num || v instanceof Str || v instanceof Closure) {
         return joinα(v.__α, e)
      } else
      if (v instanceof DataValue) {
         const eʹ: Expr.Constr = as(e, Expr.Constr)
         // reverse order but shouldn't matter in absence of side-effects:
         zip(v.fieldExplValues(), eʹ.args.toArray()).map(([[t, v], e]) => eval_bwd(e, explValue(t, v)))
         return joinα(v.__α, e)
      } else {
         return absurd()
      }
   } else
   if (t instanceof Expl.Quote) {
      return joinα(v.__α, e)
   } else
   if (t instanceof Expl.Var) {
      joinα(v.__α, asAnnotated(t.v))
      return joinα(v.__α, e)
   } else
   if (t instanceof Expl.App) {
      assert(t.tf.v instanceof Closure)
      joinα(v.__α, asAnnotated(t.tv.v))
      eval_bwd(t.ξ.κ, t.tv)
      match_bwd(t.ξ, v.__α)
      recDefs_(Direction.Bwd, t.δ)
      joinα(v.__α, asAnnotated(t.tf.v))
      const eʹ: Expr.App = as(e, Expr.App)
      eval_bwd(eʹ.f, t.tf)
      eval_bwd(eʹ.e, t.tu)
      return joinα(v.__α, e)
   } else
   if (t instanceof Expl.UnaryApp) {
      joinα(v.__α, asAnnotated(t.tf.v))
      joinα(v.__α, asAnnotated(t.tv.v))
      const eʹ: Expr.App = as(e, Expr.App)
      eval_bwd(eʹ.f, t.tf)
      eval_bwd(eʹ.e, t.tv)
      return joinα(v.__α, e)
   } else
   if (t instanceof Expl.BinaryApp) {
      assert(binaryOps.has(t.opName.val))
      joinα(v.__α, asAnnotated(t.tv1.v))
      joinα(v.__α, asAnnotated(t.tv2.v))
      const eʹ: Expr.BinaryApp = as(e, Expr.BinaryApp)
      eval_bwd(eʹ.e1, t.tv1)
      eval_bwd(eʹ.e2, t.tv2)
      return joinα(v.__α, e)
   } else
   if (t instanceof Expl.Defs) {
      joinα(v.__α, asAnnotated(t.tv.v))
      const eʹ: Expr.Defs = as(e, Expr.Defs)
      eval_bwd(eʹ.e, t.tv)
      defs_bwd(eʹ.def̅, t.def̅)
      return joinα(v.__α, e)
   } else
   if (t instanceof Expl.MatchAs) {
      joinα(v.__α, asAnnotated(t.tv.v))
      const eʹ: Expr.MatchAs = as(e, Expr.MatchAs)
      eval_bwd(t.ξ.κ, t.tv)
      match_bwd(t.ξ, v.__α)
      eval_bwd(eʹ.e, t.tu)
      return joinα(v.__α, e)
   } else
   if (t instanceof Expl.Typematch) {
      joinα(v.__α, asAnnotated(t.tv.v))
      const eʹ: Expr.Typematch = as(e, Expr.Typematch)
      eval_bwd(get(eʹ.cases, t.d)!, t.tv)
      eval_bwd(eʹ.e, t.tu)
      return joinα(v.__α, e)
   } else {
      return absurd()
   }
}

}

initDataType(
   Expr.Expr,
   [Expr.App, Expr.BinaryApp, Expr.ConstNum, Expr.ConstStr, Expr.Constr, Expr.Defs, Expr.Fun, Expr.MatchAs, Expr.Quote, Expr.Var]
)
