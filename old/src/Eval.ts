import { zip } from "./util/Array"
import { Class, __nonNull, absurd, as, assert, className, classOf, userError } from "./util/Core"
import { bool_ } from "./util/Lattice"
import { Direction, isα, setjoinα, setmeetα, setα } from "./Annotation"
import { Cons, List, Nil, cons, nil } from "./BaseTypes"
import { DataType, PrimType, ctrToDataType, explClass, initDataType, types, valueClass } from "./DataType"
import { DataValue, ExplValue, explValue } from "./DataValue"
import { Env, emptyEnv, extendEnv } from "./Env"
import { Expl } from "./Expl"
import { Expr } from "./Expr"
import { get } from "./FiniteMap"
import { Elim, Match, apply_bwd, apply_fwd } from "./Match"
import { UnaryOp, BinaryOp, binaryOps, unaryOps } from "./Primitive"
import { Id, MemoId, PrimValue, Num, Str, TaggedId, Value, _, memoId } from "./Value"
import { at, num, str } from "./Versioned"

type Def = Expr.Def
type RecDef = Expr.RecDef

export type ExplId = TaggedId<"t">
export type ValId = TaggedId<"v">

export module Eval {

// ρ plus bindings in δ are closing for f.
export class Closure extends DataValue<"Closure"> {
   ρ: Env = _
   δ: List<RecDef> = _
   f: Elim<Expr> = _
}

function closure (ρ: Env, δ: List<RecDef>, f: Elim<Expr>): (k: Id) => Closure {
   return at(Closure, ρ, δ, f)
}

// Environments are snoc-lists, so this (inconsequentially) reverses declaration order.
function recDefs (δ_0: List<RecDef>, ρ: Env, δ: List<RecDef>): [List<Expl.RecDef>, Env] {
   if (Cons.is(δ)) {
      const def: RecDef = δ.head,
            [δₜ, ρ_ext]: [List<Expl.RecDef>, Env] = recDefs(δ_0, ρ, δ.tail),
            k: MemoId = memoId(recDefs, arguments),
            tf: ExplValue<Closure> = explValue(Expl.fun(def.σ)(k.tag("t")), closure(ρ, δ_0, def.σ)(k.tag("v")))
      return [cons(Expl.recDef(def.x, tf)(k), δₜ), extendEnv(ρ_ext, def.x, tf)]
   } else
   if (Nil.is(δ)) {
      return [nil(), emptyEnv()]
   } else {
      return absurd()
   }
}

function recDefs_ (dir: Direction, δ: List<Expl.RecDef>): void {
   if (Cons.is(δ)) {
      zip(δ.head.tf.v.δ.toArray(), δ.toArray()).map(([def, defₜ]: [RecDef, Expl.RecDef]): void => {
         assert(def.x.eq(defₜ.x))
         if (dir === Direction.Fwd) {
            setα(isα(def), defₜ.tf)
         } else {
            setjoinα(isα(defₜ.tf), def)
         }
      })
   } else
   if (Nil.is(δ)) {
   } else {
      return absurd()
   }
}

// Here we mustn't invert definition order.
export function defs (ρ: Env, def̅: List<Def>, ρ_ext: Env): [List<Expl.Def>, Env] {
   const k: MemoId = memoId(defs, arguments)
   if (Cons.is(def̅)) {
      const def: Def = def̅.head
      if (def instanceof Expr.Let) {
         const tv: ExplValue = eval_(ρ.concat(ρ_ext), def.e),
               [def̅ₜ, ρ_extʹ]: [List<Expl.Def>, Env] = defs(ρ, def̅.tail, extendEnv(ρ_ext, def.x, tv))
         return [cons(Expl.let_(def.x, tv)(k), def̅ₜ), ρ_extʹ]
      } else
      if (def instanceof Expr.Prim) {
         // first-class primitives currently happen to be unary
         if (unaryOps.has(def.x.val)) {
            const t_op: ExplValue<UnaryOp> = unaryOps.get(def.x.val)!,
                  [def̅ₜ, ρ_extʹ]: [List<Expl.Def>, Env] = defs(ρ, def̅.tail, extendEnv(ρ_ext, def.x, t_op))
            return [cons(Expl.prim(def.x, t_op)(k), def̅ₜ), ρ_extʹ]
         } else {
            return userError(`No implementation found for primitive "${def.x.val}".`)
         }
      } else
      if (def instanceof Expr.LetRec) {
         const [δ, ρᵟ]: [List<Expl.RecDef>, Env] = recDefs(def.δ, ρ.concat(ρ_ext), def.δ),
               [def̅ₜ, ρ_extʹ]: [List<Expl.Def>, Env] = defs(ρ, def̅.tail, ρ_ext.concat(ρᵟ))
         return [cons(Expl.letRec(δ)(k), def̅ₜ), ρ_extʹ]
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
      if (def instanceof Expr.Let && defₜ instanceof Expl.Let) {
         eval_fwd(def.e, defₜ.tv)
         setmeetα(isα(def), defₜ.tv)
      } else
      if (def instanceof Expr.Prim && defₜ instanceof Expl.Prim) {
         setα(isα(def), defₜ.t_op)
      } else
      if (def instanceof Expr.LetRec && defₜ instanceof Expl.LetRec) {
         recDefs_(Direction.Fwd, defₜ.δ)
      } else {
         absurd()
      }
   })
}

function defs_bwd (def̅: List<Def>, def̅ₜ: List<Expl.Def>): void {
   zip(def̅.toArray(), def̅ₜ.toArray()).reverse().forEach(([def, defₜ]: [Def, Expl.Def]) => {
      if (def instanceof Expr.Let && defₜ instanceof Expl.Let) {
         setjoinα(isα(defₜ.tv), def)
         eval_bwd(def.e, defₜ.tv)
      } else
      if (def instanceof Expr.Prim && defₜ instanceof Expl.Prim) {
         setjoinα(isα(defₜ.t_op), def)
      } else
      if (def instanceof Expr.LetRec && defₜ instanceof Expl.LetRec) {
         recDefs_(Direction.Bwd, defₜ.δ)
      } else {
         absurd()
      }
   })
}

export function eval_ (ρ: Env, e: Expr): ExplValue {
   const k: MemoId = memoId(eval_, arguments),
         [kₜ, kᵥ]: [ExplId, ValId] = [k.tag("t"), k.tag("v")]
   if (e instanceof Expr.ConstNum) {
      return explValue(Expl.const_()(kₜ), num(e.val.val)(kᵥ))
   } else
   if (e instanceof Expr.ConstStr) {
      return explValue(Expl.const_()(kₜ), str(e.val.val)(kᵥ))
   } else
   if (e instanceof Expr.Fun) {
      return explValue(Expl.fun(e.σ)(kₜ), closure(ρ, nil(), e.σ)(kᵥ))
   } else
   if (e instanceof Expr.DataExpr) {
      const tv̅: ExplValue[] = e.__children.map((e: Expr) => eval_(ρ, e)),
            C: Class<DataValue> = valueClass(classOf(e)),
            t: Expl = at(explClass(C), ...tv̅.map(({t}) => t))(kₜ),
            v: Value = at(C, ...tv̅.map(({v}) => v))(kᵥ)
      return explValue(t, v)
   } else
   if (e instanceof Expr.Quote) {
      return explValue(Expl.quote()(kₜ), e.e)
   } else
   if (e instanceof Expr.Var) {
      if (ρ.has(e.x)) {
         const {t, v}: ExplValue = ρ.get(e.x)!
         return explValue(Expl.var_(e.x, t)(kₜ), v)
      } else {
         return userError(`Variable "${e.x.val}" not found.`)
      }
   } else
   if (e instanceof Expr.App) {
      const [tf, tu]: [ExplValue, ExplValue] = [eval_(ρ, e.f), eval_(ρ, e.e)],
            [v, u]: [Value, Value] = [tf.v, tu.v]
      if (v instanceof Closure) {
         const [δ, ρᵟ]: [List<Expl.RecDef>, Env] = recDefs(v.δ, v.ρ, v.δ),
               [ρʹ, ξκ]: [Env, Match<Expr>] = v.f.apply(tu),
               {t, v: vʹ}: ExplValue = eval_(v.ρ.concat(ρᵟ.concat(ρʹ)), ξκ.κ)
         return explValue(Expl.app(tf as ExplValue<Closure>, tu, δ, ξκ, t)(kₜ), vʹ)
      } else
      if (v instanceof UnaryOp) {
         if (u instanceof Num || u instanceof Str) {
            return explValue(Expl.unaryApp(tf as ExplValue<UnaryOp>, tu as ExplValue<PrimValue>)(kₜ), v.op(u)(kᵥ))
         } else {
            return userError(`Applying "${v.name}" to non-primitive value.`, u)
         }
      } else {
         return userError(`Cannot apply ${className(v)}`)
      }
   } else
   // Binary operators are (currently) "syntax", rather than first-class.
   if (e instanceof Expr.BinaryApp) {
      if (binaryOps.has(e.opName.val)) {
         const op: BinaryOp = binaryOps.get(e.opName.val)!.v,
               [tv1, tv2]: [ExplValue, ExplValue] = [eval_(ρ, e.e1), eval_(ρ, e.e2)],
               [v1, v2]: [Value, Value] = [tv1.v, tv2.v]
         if ((v1 instanceof Num || v1 instanceof Str) && (v2 instanceof Num || v2 instanceof Str)) {
            const k: MemoId = memoId(op.op, [v1, v2])
            return explValue(Expl.binaryApp(tv1 as ExplValue<PrimValue>, e.opName, tv2 as ExplValue<PrimValue>)(kₜ), op.op(v1, v2)(k))
         } else {
            return userError(`Applying "${e.opName.val}" to non-primitive value.`, v1, v2)
         }
      } else {
         return userError(`Binary primitive "${e.opName.val}" not found.`)
      }
   } else
   if (e instanceof Expr.Defs) {
      const [def̅ₜ, ρʹ]: [List<Expl.Def>, Env] = defs(ρ, e.def̅, emptyEnv()),
            {t, v}: ExplValue = eval_(ρ.concat(ρʹ), e.e)
      return explValue(Expl.defs(def̅ₜ, t)(kₜ), v)
   } else
   if (e instanceof Expr.MatchAs) {
      const tu: ExplValue = eval_(ρ, e.e),
            [ρʹ, ξκ]: [Env, Match<Expr>] = e.σ.apply(tu),
            {t, v}: ExplValue = eval_(ρ.concat(ρʹ), ξκ.κ)
      return explValue(Expl.matchAs(tu, ξκ, t)(kₜ), v)
   } else
   if (e instanceof Expr.Typematch) {
      const tu: ExplValue = eval_(ρ, e.e),
            d: DataType | PrimType = ctrToDataType.get(className(tu.v)) || types.get(className(tu.v))!,
            eʹ: Expr | undefined = get(e.cases, d.name)
      if (eʹ === undefined) {
         return userError(`Typecase mismatch: no clause for ${className(tu.v)}.`)
      } else {
         const {t, v}: ExplValue = eval_(ρ, eʹ)
         return explValue(Expl.typematch(tu, d.name, t)(kₜ), v)
      }
   } else {
      return absurd(`Unimplemented expression form: ${className(e)}.`)
   }
}

export function eval_fwd (e: Expr, tv: ExplValue): void {
   const {t, v}: ExplValue = tv
   if (t instanceof Expl.Const && (v instanceof Num || v instanceof Str)) {
      setα(isα(e), tv)
   } else
   if (t instanceof Expl.Fun && v instanceof Closure) {
      setα(isα(e), tv)
   } else
   if (t instanceof Expl.Quote) {
      setα(isα(e), tv)
   } else
   if (t instanceof Expl.Var) {
      setα(bool_.meet(isα(e), isα(explValue(t.t, v))), tv)
   } else
   if (t instanceof Expl.DataExpl) {
      if (v instanceof DataValue) {
         const eʹ: Expr.DataExpr = as(e, Expr.DataExpr)
         zip(Expl.explChildren(t, v), eʹ.__children).map(([tv, e]) => eval_fwd(e, tv))
         setα(isα(e), tv)
      } else {
         absurd()
      }
   } else
   if (t instanceof Expl.App) {
      const eʹ: Expr.App = as(e, Expr.App)
      eval_fwd(eʹ.f, t.tf)
      eval_fwd(eʹ.e, t.tu)
      recDefs_(Direction.Fwd, t.δ)
      eval_fwd(t.ξ.κ, explValue(t.t, v))
      setα(bool_.meet(isα(t.tf), apply_fwd(t.ξ), isα(e), isα(explValue(t.t, v))), tv)
   } else
   if (t instanceof Expl.UnaryApp) {
      const eʹ: Expr.App = as(e, Expr.App)
      eval_fwd(eʹ.f, t.tf)
      eval_fwd(eʹ.e, t.tv)
      setα(bool_.meet(isα(t.tf), isα(t.tv), isα(e)), tv)
   } else
   if (t instanceof Expl.BinaryApp) {
      const eʹ: Expr.BinaryApp = as(e, Expr.BinaryApp)
      eval_fwd(eʹ.e1, t.tv1)
      eval_fwd(eʹ.e2, t.tv2)
      setα(bool_.meet(isα(t.tv1), isα(t.tv2), isα(e)), tv)
   } else
   if (t instanceof Expl.Defs) {
      const eʹ: Expr.Defs = as(e, Expr.Defs)
      defs_fwd(eʹ.def̅, t.def̅)
      eval_fwd(eʹ.e, explValue(t.t, v))
      setα(bool_.meet(isα(e), isα(explValue(t.t, v))), tv)
   } else
   if (t instanceof Expl.MatchAs) {
      const eʹ: Expr.MatchAs = as(e, Expr.MatchAs)
      eval_fwd(eʹ.e, t.tu)
      eval_fwd(t.ξ.κ, explValue(t.t, v))
      setα(bool_.meet(apply_fwd(t.ξ), isα(e), isα(explValue(t.t, v))), tv)
   } else
   if (t instanceof Expl.Typematch) {
      const eʹ: Expr.Typematch = as(e, Expr.Typematch)
      eval_fwd(eʹ.e, t.tu)
      eval_fwd(get(eʹ.cases, t.d)!, explValue(t.t, v))
      setα(bool_.meet(isα(e), isα(explValue(t.t, v))), tv)
   } else {
      absurd()
   }
}

// Avoid excessive joins via a merging implementation; requires all annotations to have been cleared first.
export function eval_bwd (e: Expr, tv: ExplValue): void {
   const {t, v}: ExplValue = tv
   if (t instanceof Expl.Const && (v instanceof Num || v instanceof Str)) {
      setjoinα(isα(tv), e)
   } else
   if (t instanceof Expl.Fun && v instanceof Closure) {
      setjoinα(isα(tv), e)
   } else
   if (t instanceof Expl.DataExpl) {
      if (v instanceof DataValue) {
         const eʹ: Expr.DataExpr = as(e, Expr.DataExpr)
         // reverse order but shouldn't matter in absence of side-effects:
         zip(Expl.explChildren(t, v), eʹ.__children).map(([tv, e]) => eval_bwd(e, tv))
         setjoinα(isα(tv), e)
      } else {
         absurd()
      }
   } else
   if (t instanceof Expl.Quote) {
      setjoinα(isα(tv), e)
   } else
   if (t instanceof Expl.Var) {
      setjoinα(isα(tv), explValue(t.t, v))
      setjoinα(isα(tv), e)
   } else
   if (t instanceof Expl.App) {
      assert(t.tf.v instanceof Closure)
      setjoinα(isα(tv), explValue(t.t, v))
      eval_bwd(t.ξ.κ, explValue(t.t, v))
      apply_bwd(t.ξ, isα(tv))
      recDefs_(Direction.Bwd, t.δ)
      setjoinα(isα(tv), t.tf)
      const eʹ: Expr.App = as(e, Expr.App)
      eval_bwd(eʹ.f, t.tf)
      eval_bwd(eʹ.e, t.tu)
      setjoinα(isα(tv), e)
   } else
   if (t instanceof Expl.UnaryApp) {
      setjoinα(isα(tv), t.tf)
      setjoinα(isα(tv), t.tv)
      const eʹ: Expr.App = as(e, Expr.App)
      eval_bwd(eʹ.f, t.tf)
      eval_bwd(eʹ.e, t.tv)
      setjoinα(isα(tv), e)
   } else
   if (t instanceof Expl.BinaryApp) {
      assert(binaryOps.has(t.opName.val))
      setjoinα(isα(tv), t.tv1)
      setjoinα(isα(tv), t.tv2)
      const eʹ: Expr.BinaryApp = as(e, Expr.BinaryApp)
      eval_bwd(eʹ.e1, t.tv1)
      eval_bwd(eʹ.e2, t.tv2)
      setjoinα(isα(tv), e)
   } else
   if (t instanceof Expl.Defs) {
      setjoinα(isα(tv), explValue(t.t, v))
      const eʹ: Expr.Defs = as(e, Expr.Defs)
      eval_bwd(eʹ.e, explValue(t.t, v))
      defs_bwd(eʹ.def̅, t.def̅)
      setjoinα(isα(tv), e)
   } else
   if (t instanceof Expl.MatchAs) {
      setjoinα(isα(tv), explValue(t.t, v))
      const eʹ: Expr.MatchAs = as(e, Expr.MatchAs)
      eval_bwd(t.ξ.κ, explValue(t.t, v))
      apply_bwd(t.ξ, isα(tv))
      eval_bwd(eʹ.e, t.tu)
      setjoinα(isα(tv), e)
   } else
   if (t instanceof Expl.Typematch) {
      setjoinα(isα(tv), explValue(t.t, v))
      const eʹ: Expr.Typematch = as(e, Expr.Typematch)
      eval_bwd(get(eʹ.cases, t.d)!, explValue(t.t, v))
      eval_bwd(eʹ.e, t.tu)
      setjoinα(isα(tv), e)
   } else {
      absurd()
   }
}

}

initDataType(
   Expr.Expr,
   [Expr.App, Expr.BinaryApp, Expr.ConstNum, Expr.ConstStr, Expr.DataExpr, Expr.Defs, Expr.Fun, Expr.MatchAs, Expr.Quote, Expr.Var]
)
