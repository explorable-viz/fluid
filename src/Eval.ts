import { ann } from "./util/Annotated"
import { zip } from "./util/Array"
import { __nonNull, absurd, assert, className, error } from "./util/Core"
import { Cons, List, Nil, cons, nil } from "./BaseTypes"
import { DataType, ctrToDataType, initDataType } from "./DataType"
import { DataValue } from "./DataValue"
import { Env, emptyEnv, extendEnv } from "./Env"
import { Expl, ExplValue, explValue } from "./ExplValue"
import { Expr } from "./Expr"
import { Elim, Match, evalTrie, match_bwd, match_fwd } from "./Match"
import { UnaryOp, BinaryOp, binaryOps, unaryOps } from "./Primitive"
import { Id, Num, Str, TaggedId, Value, _, make, memoId, taggedId } from "./Value"
import { Versioned, VersionedC, at, copyAt, joinα, meetα, numʹ, setα, strʹ } from "./Versioned"

export enum Direction { Fwd, Bwd }
type Def = Expr.Def
type RecDef = Expr.RecDef

export type ValId = TaggedId<"v">
export type ExplId = TaggedId<"t">
export type ListId = TaggedId<"l">

export module Eval {

// ρ plus bindings in δ are closing for f.
export class Closure extends VersionedC(DataValue)<"Closure"> {
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
            kᵥ: ValId = taggedId(memoId(recDefs, arguments), "v"),
            kₗ: ListId = taggedId(memoId(recDefs, arguments), "l"),
            f: Closure = closure(kᵥ, ρ, δ_0, evalTrie(def.σ))
      return [cons(kₗ, Expl.recDef(def.x, f), δₜ), extendEnv(ρ_ext, def.x, f)]
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
   const kₗ: ListId = taggedId(memoId(defs, arguments), "l")
   if (Cons.is(def̅)) {
      const def: Def = def̅.head
      if (def instanceof Expr.Let) {
         const tv: ExplValue = eval_(ρ.concat(ρ_ext), def.e),
               [def̅ₜ, ρ_extʹ]: [List<Expl.Def>, Env] = defs(ρ, def̅.tail, extendEnv(ρ_ext, def.x, tv.v))
         return [cons(kₗ, Expl.let_(def.x, tv), def̅ₜ), ρ_extʹ]
      } else
      if (def instanceof Expr.Prim) {
         // first-class primitives currently happen to be unary
         if (unaryOps.has(def.x.val)) {
            const op: Versioned<UnaryOp> = unaryOps.get(def.x.val)!,
                  [def̅ₜ, ρ_extʹ]: [List<Expl.Def>, Env] = defs(ρ, def̅.tail, extendEnv(ρ_ext, def.x, op))
            return [cons(kₗ, Expl.prim(def.x, op), def̅ₜ), ρ_extʹ]
         } else {
            return error(`No implementation found for primitive "${def.x.val}".`)
         }
      } else
      if (def instanceof Expr.LetRec) {
         const [δ, ρᵟ]: [List<Expl.RecDef>, Env] = recDefs(def.δ, ρ.concat(ρ_ext), def.δ),
               [def̅ₜ, ρ_extʹ]: [List<Expl.Def>, Env] = defs(ρ, def̅.tail, ρ_ext.concat(ρᵟ))
         return [cons(kₗ, Expl.letRec(δ), def̅ₜ), ρ_extʹ]
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

function defs_fwd (def̅: List<Expl.Def>): void {
   def̅.toArray().forEach((def: Expl.Def) => {
      if (def instanceof Expl.Let) {
         eval_fwd(def.tv)
         meetα(def.x.__α, def.tv.v)
      } else
      if (def instanceof Expl.Prim) {
         setα(def.x.__α, def.op)
      } else
      if (def instanceof Expl.LetRec) {
         recDefs_(Direction.Fwd, def.δ)
      } else {
         absurd()
      }
   })
}

function defs_bwd (def̅: List<Expl.Def>): void {
   def̅.toArray().reverse().forEach((def: Expl.Def) => {
      if (def instanceof Expl.Let) {
         joinα(def.tv.v.__α, def.x)
         eval_bwd(def.tv)
      } else
      if (def instanceof Expl.Prim) {
         joinα(def.op.__α, def.x)
      } else
      if (def instanceof Expl.LetRec) {
         recDefs_(Direction.Bwd, def.δ)
      } else {
         absurd()
      }
   })
}

export function eval_ (ρ: Env, e: Expr): ExplValue {
   const kₜ: ExplId = taggedId(memoId(eval_, arguments), "t"),
         kᵥ: ValId = taggedId(memoId(eval_, arguments), "v")
   if (e instanceof Expr.ConstNum) {
      return explValue(Expl.empty(kₜ), numʹ(kᵥ, e.val.val))
   } else
   if (e instanceof Expr.ConstStr) {
      return explValue(Expl.empty(kₜ), strʹ(kᵥ, e.val.val))
   } else
   if (e instanceof Expr.Fun) {
      return explValue(Expl.empty(kₜ), closure(kᵥ, ρ, nil(), evalTrie(e.σ)))
   } else
   if (e instanceof Expr.Constr) {
      let tv̅: ExplValue[] = e.args.toArray().map((e: Expr) => eval_(ρ, e)),
          c: string = e.ctr.val,
          d: DataType = __nonNull(ctrToDataType.get(c)),
          v: Versioned<DataValue> = at(kᵥ, d.ctrs.get(c)!.C, ...tv̅.map(({v}) => v))
      v.__expl = make(d.explC̅.get(c)!, ...tv̅.map(({t}) => t))
      return explValue(Expl.empty(kₜ), v)
   } else
   if (e instanceof Expr.Quote) {
      return explValue(Expl.quote(kₜ), copyAt(kᵥ, e.e))
   } else
   if (e instanceof Expr.Var) {
      if (ρ.has(e.x)) { 
         const v: Versioned<Value> = ρ.get(e.x)!
         return explValue(Expl.var_(kₜ, e.x, v), copyAt(kᵥ, v))
      } else {
         return error(`Variable "${e.x.val}" not found.`)
      }
   } else
   if (e instanceof Expr.App) {
      const [tf, tu]: [ExplValue, ExplValue] = [eval_(ρ, e.f), eval_(ρ, e.e)],
            [v, u]: [Value, Versioned<Value>] = [tf.v, tu.v]
      if (v instanceof Closure) {
         const [δ, ρᵟ]: [List<Expl.RecDef>, Env] = recDefs(v.δ, v.ρ, v.δ),
               [ρʹ, ξ, eʹ]: [Env, Match, Expr] = v.f.match(u, nil()),
               tv: ExplValue = eval_(v.ρ.concat(ρᵟ.concat(ρʹ)), eʹ)
         return explValue(Expl.app(kₜ, tf, tu, δ, ξ, tv), copyAt(kᵥ, tv.v))
      } else 
      if (v instanceof UnaryOp) {
         if (u instanceof Num || u instanceof Str) {
            return explValue(Expl.unaryApp(kₜ, tf, tu), v.op(u)(kᵥ))
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
         const op: BinaryOp = binaryOps.get(e.opName.val)!, // TODO: add annotations to opName
               [tv1, tv2]: [ExplValue, ExplValue] = [eval_(ρ, e.e1), eval_(ρ, e.e2)],
               [v1, v2]: [Versioned<Value>, Versioned<Value>] = [tv1.v, tv2.v]
         if ((v1 instanceof Num || v1 instanceof Str) && (v2 instanceof Num || v2 instanceof Str)) {
               return explValue(Expl.binaryApp(kₜ, tv1, e.opName, tv2), op.op(v1, v2)(kᵥ))
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
      return explValue(Expl.defs(kₜ, def̅ₜ, tv), copyAt(kᵥ, tv.v))
   } else
   if (e instanceof Expr.MatchAs) {
      const tu: ExplValue = eval_(ρ, e.e),
            [ρʹ, ξ, eʹ]: [Env, Match, Expr] = evalTrie(e.σ).match(tu.v, nil()),
            tv: ExplValue = eval_(ρ.concat(ρʹ), eʹ)
      return explValue(Expl.matchAs(kₜ, tu, ξ, tv), copyAt(kᵥ, tv.v))
   } else {
      return absurd(`Unimplemented expression form: ${className(e)}.`)
   }
}

function toExpr (t: Expl): Expr {
   return (t.__id as ExplId).k.args[1] as Expr
}

export function eval_fwd ({t, v}: ExplValue): void {
   const e: Expr = toExpr(t)
   if (t instanceof Expl.Empty) {
      if (v instanceof Num || v instanceof Str || v instanceof Closure) {
         setα(e.__α, v)
      } else
      if (v instanceof DataValue) {
         v.fieldExplValues().map(([t, v]) => eval_fwd(explValue(t, v)))
         setα(e.__α, v)
      }
   } else
   if (t instanceof Expl.Quote) {
      setα(e.__α, v)
   } else
   if (t instanceof Expl.Var) {
      setα(ann.meet(e.__α, t.v.__α), v)
   } else
   if (t instanceof Expl.App) {
      eval_fwd(t.tf)
      eval_fwd(t.tu)
      recDefs_(Direction.Fwd, t.δ)
      eval_fwd(t.tv)
      setα(ann.meet(t.tf.v.__α, match_fwd(t.ξ), e.__α, t.tv.v.__α), v)
   } else
   if (t instanceof Expl.UnaryApp) {
      eval_fwd(t.tf)
      eval_fwd(t.tv)
      setα(ann.meet(t.tf.v.__α, t.tv.v.__α, e.__α), v)
   } else
   if (t instanceof Expl.BinaryApp) {
      eval_fwd(t.tv1)
      eval_fwd(t.tv2)
      setα(ann.meet(t.tv1.v.__α, t.tv2.v.__α, e.__α), v)
   } else
   if (t instanceof Expl.Defs) {
      defs_fwd(t.def̅)
      eval_fwd(t.tv)
      setα(ann.meet(e.__α, t.tv.v.__α), v)
   } else
   if (t instanceof Expl.MatchAs) {
      eval_fwd(t.tu)
      eval_fwd(t.tv)
      setα(ann.meet(match_fwd(t.ξ), e.__α, t.tv.v.__α), v)
   } else {
      absurd()
   }
}

// Avoid excessive joins via a merging implementation; requires all annotations to have been cleared first.
export function eval_bwd ({t, v}: ExplValue): Expr {
   const e: Expr = toExpr(t)
   if (t instanceof Expl.Empty) {
      if (v instanceof Num || v instanceof Str || v instanceof Closure) {
         return joinα(v.__α, e)
      } else
      if (v instanceof DataValue) {
         // reverse order but shouldn't matter in absence of side-effects:
         v.fieldExplValues().map(([t, v]) => eval_bwd(explValue(t, v)))
         return joinα(v.__α, e)
      } else {
         return absurd()
      }
   } else
   if (t instanceof Expl.Quote) {
      return joinα(v.__α, e)
   } else
   if (t instanceof Expl.Var) {
      joinα(v.__α, t.v)
      return joinα(v.__α, e)
   } else
   if (t instanceof Expl.App) {
      assert(t.tf.v instanceof Closure)
      joinα(v.__α, t.tv.v)
      eval_bwd(t.tv)
      match_bwd(t.ξ, v.__α)
      recDefs_(Direction.Bwd, t.δ)
      joinα(v.__α, t.tf.v)
      eval_bwd(t.tf)
      eval_bwd(t.tu)
      return joinα(v.__α, e)
   } else
   if (t instanceof Expl.UnaryApp) {
      joinα(v.__α, t.tf.v)
      joinα(v.__α, t.tv.v)
      eval_bwd(t.tf)
      eval_bwd(t.tv)
      return joinα(v.__α, e)
   } else
   if (t instanceof Expl.BinaryApp) {
      assert(binaryOps.has(t.opName.val))
      joinα(v.__α, t.tv1.v)
      joinα(v.__α, t.tv2.v)
      eval_bwd(t.tv1)
      eval_bwd(t.tv2)
      return joinα(v.__α, e)
   } else
   if (t instanceof Expl.Defs) {
      joinα(v.__α, t.tv.v)
      eval_bwd(t.tv)
      defs_bwd(t.def̅)
      return joinα(v.__α, e)
   } else
   if (t instanceof Expl.MatchAs) {
      joinα(v.__α, t.tv.v)
      eval_bwd(t.tv)
      match_bwd(t.ξ, v.__α)
      eval_bwd(t.tu)
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
