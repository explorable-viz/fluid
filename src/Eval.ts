import { zip } from "./util/Array"
import { __nonNull, absurd, as, assert, className, error } from "./util/Core"
import { ann } from "./util/Lattice"
import { Annotated, AnnotatedC, annotatedAt, joinα, meetα, num, setα, str } from "./Annotated"
import { Cons, List, Nil, cons, nil } from "./BaseTypes"
import { DataType, PrimType, ctrToDataType, initDataType, types } from "./DataType"
import { DataValue } from "./DataValue"
import { Env, emptyEnv, extendEnv } from "./Env"
import { Expl, Expl_, expl } from "./Expl"
import { Expr } from "./Expr"
import { get } from "./FiniteMap"
import { Elim, Match, evalTrie, match_bwd, match_fwd } from "./Match"
import { UnaryOp, BinaryOp, binaryOps, unaryOps } from "./Primitive"
import { Id, PrimValue, Num, Str, Value, _, make } from "./Value"
import { ν, at, copyAt } from "./Versioned"

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
            tf: Expl_<Closure> = expl(Expl.empty(), closure(ν(), ρ, δ_0, evalTrie(def.σ)))
      return [cons(Expl.recDef(def.x, tf), δₜ), extendEnv(ρ_ext, def.x, tf.v)]
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
            setα(def.x.__α, defₜ.tf.v)
            setα(def.x.__α, defₜ.tf.t)
         } else {
            joinα(defₜ.tf.v.__α, def.x)
            joinα(defₜ.tf.t.__α, def.x)
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
         const tv: Expl_ = eval_(ρ.concat(ρ_ext), def.e),
               [def̅ₜ, ρ_extʹ]: [List<Expl.Def>, Env] = defs(ρ, def̅.tail, extendEnv(ρ_ext, def.x, tv.v))
         return [cons(Expl.let_(def.x, tv), def̅ₜ), ρ_extʹ]
      } else
      if (def instanceof Expr.Prim) {
         // first-class primitives currently happen to be unary
         if (unaryOps.has(def.x.val)) {
            const t_op: Expl_<UnaryOp> = unaryOps.get(def.x.val)!,
                  [def̅ₜ, ρ_extʹ]: [List<Expl.Def>, Env] = defs(ρ, def̅.tail, extendEnv(ρ_ext, def.x, t_op.v))
            return [cons(Expl.prim(def.x, t_op), def̅ₜ), ρ_extʹ]
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
      if (def instanceof Expr.Let && defₜ instanceof Expl.Let) {
         eval_fwd(def.e, defₜ.tv)
         meetα(def.x.__α, defₜ.tv.v)
         meetα(def.x.__α, defₜ.tv.t)
      } else
      if (def instanceof Expr.Prim && defₜ instanceof Expl.Prim) {
         setα(def.x.__α, defₜ.t_op.v)
         setα(def.x.__α, defₜ.t_op.t)
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
         joinα(defₜ.tv.v.__α, def.x)
         joinα(defₜ.tv.t.__α, def.x)
         eval_bwd(def.e, defₜ.tv)
      } else
      if (def instanceof Expr.Prim && defₜ instanceof Expl.Prim) {
         joinα(defₜ.t_op.v.__α, def.x)
         joinα(defₜ.t_op.t.__α, def.x)
      } else
      if (def instanceof Expr.LetRec && defₜ instanceof Expl.LetRec) {
         recDefs_(Direction.Bwd, defₜ.δ)
      } else {
         absurd()
      }
   })
}

export function eval_ (ρ: Env, e: Expr): Expl_ {
   if (e instanceof Expr.ConstNum) {
      return expl(Expl.empty(), num(e.val.val))
   } else
   if (e instanceof Expr.ConstStr) {
      return expl(Expl.empty(), str(e.val.val))
   } else
   if (e instanceof Expr.Fun) {
      return expl(Expl.empty(), closure(ν(), ρ, nil(), evalTrie(e.σ)))
   } else
   if (e instanceof Expr.Constr) {
      const tv̅: Expl_[] = e.args.toArray().map((e: Expr) => eval_(ρ, e)),
            c: string = e.ctr.val,
            d: DataType = __nonNull(ctrToDataType.get(c)),
            v: Annotated<DataValue> = annotatedAt(ν(), d.ctrs.get(c)!.C, ...tv̅.map(({v}) => v))
      v.__expl = make(d.explC̅.get(c)!, ...tv̅.map(({t}) => t))
      return expl(Expl.empty(), v)
   } else
   if (e instanceof Expr.Quote) {
      return expl(Expl.quote(), copyAt(ν(), e.e))
   } else
   if (e instanceof Expr.Var) {
      if (ρ.has(e.x)) {
         const v: Annotated<Value> = ρ.get(e.x)!
         return expl(Expl.var_(e.x, v), copyAt(ν(), v))
      } else {
         return error(`Variable "${e.x.val}" not found.`)
      }
   } else
   if (e instanceof Expr.App) {
      const [tf, tu]: [Expl_, Expl_] = [eval_(ρ, e.f), eval_(ρ, e.e)],
            [v, u]: [Value, Value] = [tf.v, tu.v]
      if (v instanceof Closure) {
         const [δ, ρᵟ]: [List<Expl.RecDef>, Env] = recDefs(v.δ, v.ρ, v.δ),
               [ρʹ, ξκ]: [Env, Match<Expr>] = v.f.apply(u),
               tv: Expl_ = eval_(v.ρ.concat(ρᵟ.concat(ρʹ)), ξκ.κ)
         return expl(Expl.app(tf as Expl_<Closure>, tu, δ, ξκ, tv), copyAt(ν(), tv.v))
      } else 
      if (v instanceof UnaryOp) {
         if (u instanceof Num || u instanceof Str) {
            return expl(Expl.unaryApp(tf as Expl_<UnaryOp>, tu as Expl_<PrimValue>), v.op(u))
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
         const op: BinaryOp = binaryOps.get(e.opName.val)!.v,
               [tv1, tv2]: [Expl_, Expl_] = [eval_(ρ, e.e1), eval_(ρ, e.e2)],
               [v1, v2]: [Value, Value] = [tv1.v, tv2.v]
         if ((v1 instanceof Num || v1 instanceof Str) && (v2 instanceof Num || v2 instanceof Str)) {
               return expl(Expl.binaryApp(tv1 as Expl_<PrimValue>, e.opName, tv2 as Expl_<PrimValue>), op.op(v1, v2))
         } else {
            return error(`Applying "${e.opName}" to non-primitive value.`, v1, v2)
         }
      } else {
         return error(`Binary primitive "${e.opName.val}" not found.`)
      }
   } else
   if (e instanceof Expr.Defs) {
      const [def̅ₜ, ρʹ]: [List<Expl.Def>, Env] = defs(ρ, e.def̅, emptyEnv()),
            tv: Expl_ = eval_(ρ.concat(ρʹ), e.e)
      return expl(Expl.defs(def̅ₜ, tv), copyAt(ν(), tv.v))
   } else
   if (e instanceof Expr.MatchAs) {
      const tu: Expl_ = eval_(ρ, e.e),
            [ρʹ, ξκ]: [Env, Match<Expr>] = evalTrie(e.σ).apply(tu.v),
            tv: Expl_ = eval_(ρ.concat(ρʹ), ξκ.κ)
      return expl(Expl.matchAs(tu, ξκ, tv), copyAt(ν(), tv.v))
   } else
   if (e instanceof Expr.Typematch) {
      const tu: Expl_ = eval_(ρ, e.e),
            d: DataType | PrimType = ctrToDataType.get(className(tu.v)) || types.get(className(tu.v))!,
            eʹ: Expr | undefined = get(e.cases, d.name)
      if (eʹ === undefined) {
         return error(`Typecase mismatch: no clause for ${className(tu.v)}.`)
      } else {
         const tv: Expl_ = eval_(ρ, eʹ)
         return expl(Expl.typematch(tu, d.name, tv), copyAt(ν(), tv.v))
      }
   } else {
      return absurd(`Unimplemented expression form: ${className(e)}.`)
   }
}

export function eval_fwd (e: Expr, {t, v}: Expl_): void {
   if (t instanceof Expl.Empty) {
      if (v instanceof Num || v instanceof Str || v instanceof Closure) {
         setα(e.__α, v)
         setα(e.__α, t)
      } else
      if (v instanceof DataValue) {
         const eʹ: Expr.Constr = as(e, Expr.Constr)
         zip(v.fieldExplValues(), eʹ.args.toArray()).map(([[t, v], e]) => eval_fwd(e, expl(t, v)))
         setα(e.__α, v)
         setα(e.__α, t)
      }
   } else
   if (t instanceof Expl.Quote) {
      setα(e.__α, v)
      setα(e.__α, t)
   } else
   if (t instanceof Expl.Var) {
      setα(ann.meet(e.__α, t.v.__α), v)
      setα(ann.meet(e.__α, t.v.__α), t)
   } else
   if (t instanceof Expl.App) {
      const eʹ: Expr.App = as(e, Expr.App)
      eval_fwd(eʹ.f, t.tf)
      eval_fwd(eʹ.e, t.tu)
      recDefs_(Direction.Fwd, t.δ)
      eval_fwd(t.ξ.κ, t.tv)
      setα(ann.meet(t.tf.v.__α, match_fwd(t.ξ), e.__α, t.tv.v.__α), v)
      setα(ann.meet(t.tf.v.__α, match_fwd(t.ξ), e.__α, t.tv.v.__α), t)
   } else
   if (t instanceof Expl.UnaryApp) {
      const eʹ: Expr.App = as(e, Expr.App)
      eval_fwd(eʹ.f, t.tf)
      eval_fwd(eʹ.e, t.tv)
      setα(ann.meet(t.tf.v.__α, t.tv.v.__α, e.__α), v)
      setα(ann.meet(t.tf.v.__α, t.tv.v.__α, e.__α), t)
   } else
   if (t instanceof Expl.BinaryApp) {
      const eʹ: Expr.BinaryApp = as(e, Expr.BinaryApp)
      eval_fwd(eʹ.e1, t.tv1)
      eval_fwd(eʹ.e2, t.tv2)
      setα(ann.meet(t.tv1.v.__α, t.tv2.v.__α, e.__α), v)
      setα(ann.meet(t.tv1.v.__α, t.tv2.v.__α, e.__α), t)
   } else
   if (t instanceof Expl.Defs) {
      const eʹ: Expr.Defs = as(e, Expr.Defs)
      defs_fwd(eʹ.def̅, t.def̅)
      eval_fwd(eʹ.e, t.tv)
      setα(ann.meet(e.__α, t.tv.v.__α), v)
      setα(ann.meet(e.__α, t.tv.v.__α), t)
   } else
   if (t instanceof Expl.MatchAs) {
      const eʹ: Expr.MatchAs = as(e, Expr.MatchAs)
      eval_fwd(eʹ.e, t.tu)
      eval_fwd(t.ξ.κ, t.tv)
      setα(ann.meet(match_fwd(t.ξ), e.__α, t.tv.v.__α), v)
      setα(ann.meet(match_fwd(t.ξ), e.__α, t.tv.v.__α), t)
   } else
   if (t instanceof Expl.Typematch) {
      const eʹ: Expr.Typematch = as(e, Expr.Typematch)
      eval_fwd(eʹ.e, t.tu)
      eval_fwd(get(eʹ.cases, t.d)!, t.tv)
      setα(ann.meet(e.__α, t.tv.v.__α), v)
      setα(ann.meet(e.__α, t.tv.v.__α), t)
   } else {
      absurd()
   }
}

// Avoid excessive joins via a merging implementation; requires all annotations to have been cleared first.
export function eval_bwd (e: Expr, {t, v}: Expl_): void {
   if (t instanceof Expl.Empty) {
      if (v instanceof Num || v instanceof Str || v instanceof Closure) {
         joinα(v.__α, e)
         joinα(t.__α, e)
      } else
      if (v instanceof DataValue) {
         const eʹ: Expr.Constr = as(e, Expr.Constr)
         // reverse order but shouldn't matter in absence of side-effects:
         zip(v.fieldExplValues(), eʹ.args.toArray()).map(([[t, v], e]) => eval_bwd(e, expl(t, v)))
         joinα(v.__α, e)
         joinα(t.__α, e)
      } else {
         absurd()
      }
   } else
   if (t instanceof Expl.Quote) {
      joinα(v.__α, e)
      joinα(t.__α, e)
   } else
   if (t instanceof Expl.Var) {
      joinα(v.__α, t.v)
      joinα(t.__α, t.v)
      joinα(v.__α, e)
      joinα(t.__α, e)
   } else
   if (t instanceof Expl.App) {
      assert(t.tf.v instanceof Closure)
      joinα(v.__α, t.tv.v)
      joinα(t.__α, t.tv.t)
      eval_bwd(t.ξ.κ, t.tv)
      match_bwd(t.ξ, v.__α)
      recDefs_(Direction.Bwd, t.δ)
      joinα(v.__α, t.tf.v)
      joinα(t.__α, t.tf.t)
      const eʹ: Expr.App = as(e, Expr.App)
      eval_bwd(eʹ.f, t.tf)
      eval_bwd(eʹ.e, t.tu)
      joinα(v.__α, e)
      joinα(t.__α, e)
   } else
   if (t instanceof Expl.UnaryApp) {
      joinα(v.__α, t.tf.v)
      joinα(t.__α, t.tf.t)
      joinα(v.__α, t.tv.v)
      joinα(t.__α, t.tv.t)
      const eʹ: Expr.App = as(e, Expr.App)
      eval_bwd(eʹ.f, t.tf)
      eval_bwd(eʹ.e, t.tv)
      joinα(v.__α, e)
      joinα(t.__α, e)
   } else
   if (t instanceof Expl.BinaryApp) {
      assert(binaryOps.has(t.opName.val))
      joinα(v.__α, t.tv1.v)
      joinα(t.__α, t.tv1.t)
      joinα(v.__α, t.tv2.v)
      joinα(t.__α, t.tv2.t)
      const eʹ: Expr.BinaryApp = as(e, Expr.BinaryApp)
      eval_bwd(eʹ.e1, t.tv1)
      eval_bwd(eʹ.e2, t.tv2)
      joinα(v.__α, e)
      joinα(t.__α, e)
   } else
   if (t instanceof Expl.Defs) {
      joinα(v.__α, t.tv.v)
      joinα(t.__α, t.tv.t)
      const eʹ: Expr.Defs = as(e, Expr.Defs)
      eval_bwd(eʹ.e, t.tv)
      defs_bwd(eʹ.def̅, t.def̅)
      joinα(v.__α, e)
      joinα(t.__α, e)
   } else
   if (t instanceof Expl.MatchAs) {
      joinα(v.__α, t.tv.v)
      joinα(t.__α, t.tv.t)
      const eʹ: Expr.MatchAs = as(e, Expr.MatchAs)
      eval_bwd(t.ξ.κ, t.tv)
      match_bwd(t.ξ, v.__α)
      eval_bwd(eʹ.e, t.tu)
      joinα(v.__α, e)
      joinα(t.__α, e)
   } else
   if (t instanceof Expl.Typematch) {
      joinα(v.__α, t.tv.v)
      joinα(t.__α, t.tv.t)
      const eʹ: Expr.Typematch = as(e, Expr.Typematch)
      eval_bwd(get(eʹ.cases, t.d)!, t.tv)
      eval_bwd(eʹ.e, t.tu)
      joinα(v.__α, e)
      joinα(t.__α, e)
   } else {
      absurd()
   }
}

}

initDataType(
   Expr.Expr,
   [Expr.App, Expr.BinaryApp, Expr.ConstNum, Expr.ConstStr, Expr.Constr, Expr.Defs, Expr.Fun, Expr.MatchAs, Expr.Quote, Expr.Var]
)
