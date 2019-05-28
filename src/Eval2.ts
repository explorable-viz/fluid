import { ann } from "./util/Annotated2"
import { zip } from "./util/Array"
import { __nonNull, absurd, assert, className, error } from "./util/Core"
import { Cons, List, Nil, cons, nil } from "./BaseTypes2"
import { DataType, ctrToDataType } from "./DataType2"
import { DataValue } from "./DataValue2"
import { Env, emptyEnv, extendEnv } from "./Env2"
import { Expl, ExplValue, explValue } from "./ExplValue2"
import { Expr } from "./Expr2"
import { instantiate, instantiate_bwd, instantiate_fwd } from "./Instantiate2"
import { Elim, Match, evalTrie, match_bwd, match_fwd } from "./Match2"
import { UnaryOp, BinaryOp, binaryOps, unaryOps } from "./Primitive2"
import { Id, Num, Str, Value, _, make } from "./Value2"
import { Versioned, VersionedC, at, copyAt, joinα, numʹ, setα, strʹ } from "./Versioned2"

export enum Direction { Fwd, Bwd }
type Def = Expr.Def
type RecDef = Expr.RecDef

export class EvalId<P extends keyof ExplValue> extends Id {
   e: Expr | Versioned<Str> = _ // str case for binding occurrences of variables
   f: P = _
}

function evalId<P extends keyof ExplValue> (e: Expr | Versioned<Str>, f: P): EvalId<P> {
   return make(EvalId, e, f) as EvalId<P>
}

export type ValId = EvalId<"v">
export type ExplId = EvalId<"t">

function explId (e: Expr | Versioned<Str>): ExplId {
   return evalId(e, "t")
}

function valId (e: Expr | Versioned<Str>): ValId {
   return evalId(e, "v")
}

export module Eval {

export class Closure extends VersionedC(DataValue)<"Closure"> {
   ρ: Env = _ // ρ not closing for f; need to extend with the bindings in δ
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
            k: ValId = valId(def.x),
            [δₜ, ρ_ext]: [List<Expl.RecDef>, Env] = recDefs(δ_0, ρ, δ.tail),
            f: Closure = closure(k, ρ, δ_0, evalTrie(def.σ))
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
         const k: ValId = valId(def.x),
               tv: ExplValue = eval_(ρ.concat(ρ_ext), instantiate(ρ_ext, def.e)),
               v: Versioned<Value> = copyAt(k, tv.v),
               [def̅ₜ, ρ_extʹ]: [List<Expl.Def>, Env] = defs(ρ, def̅.tail, extendEnv(ρ_ext, def.x, v))
         return [cons(Expl.let_(def.x, tv, v), def̅ₜ), ρ_extʹ]
      } else
      if (def instanceof Expr.Prim) {
         // first-class primitives currently happen to be unary
         if (unaryOps.has(def.x.val)) {
            const k: ValId = valId(def.x),
                  op: UnaryOp = unaryOps.get(def.x.val)!,
                  opʹ: Versioned<UnaryOp> = copyAt(k, op),
                  [def̅ₜ, ρ_extʹ]: [List<Expl.Def>, Env] = defs(ρ, def̅.tail, extendEnv(ρ_ext, def.x, opʹ))
            return [cons(Expl.prim(def.x, op, opʹ), def̅ₜ), ρ_extʹ]
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

function defs_fwd (def̅: List<Expl.Def>): void {
   def̅.toArray().forEach((def: Expl.Def) => {
      if (def instanceof Expl.Let) {
         instantiate_fwd(toExpr(def.tv.t))
         eval_fwd(def.tv)
         setα(ann.meet(def.x.__α, def.tv.v.__α), def.v)
      } else
      if (def instanceof Expl.Prim) {
         setα(def.x.__α, def.opʹ)
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
         joinα(def.v.__α, def.tv.v)
         joinα(def.v.__α, def.x)
         instantiate_bwd(eval_bwd(def.tv))
      } else
      if (def instanceof Expl.Prim) {
         joinα(def.opʹ.__α, def.x)
      } else
      if (def instanceof Expl.LetRec) {
         recDefs_(Direction.Bwd, def.δ)
      } else {
         absurd()
      }
   })
}

export function eval_ (ρ: Env, e: Expr): ExplValue {
   const kₜ: ExplId = explId(e),
         kᵥ: ValId = valId(e)
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
            [v, u]: [Versioned<Value>, Versioned<Value>] = [tf.v, tu.v]
      if (v instanceof Closure) {
         const [δ, ρᵟ]: [List<Expl.RecDef>, Env] = recDefs(v.δ, v.ρ, v.δ),
               [ρʹ, ξ, eʹ]: [Env, Match, Expr] = v.f.match(u, nil()),
               ρᶠ: Env = ρᵟ.concat(ρʹ),
               tv: ExplValue = eval_(v.ρ.concat(ρᶠ), instantiate(ρᶠ, eʹ))
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
            tv: ExplValue = eval_(ρ.concat(ρʹ), instantiate(ρʹ, e.e))
      return explValue(Expl.defs(kₜ, def̅ₜ, tv), copyAt(kᵥ, tv.v))
   } else
   if (e instanceof Expr.MatchAs) {
      const tu: ExplValue = eval_(ρ, e.e),
            [ρʹ, ξ, eʹ]: [Env, Match, Expr] = evalTrie(e.σ).match(tu.v, nil()),
            tv: ExplValue = eval_(ρ.concat(ρʹ), instantiate(ρʹ, eʹ))
      return explValue(Expl.matchAs(kₜ, tu, ξ, tv), copyAt(kᵥ, tv.v))
   } else {
      return absurd(`Unimplemented expression form: ${className(e)}.`)
   }
}

function toExpr (t: Expl): Expr {
   return (t.__id as ExplId).e as Expr
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
   if (t instanceof Expl.Var) {
      setα(ann.meet(e.__α, t.v.__α), v)
   } else
   if (t instanceof Expl.App) {
      eval_fwd(t.tf)
      eval_fwd(t.tu)
      recDefs_(Direction.Fwd, t.δ)
      instantiate_fwd(toExpr(t.tv.t))
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
      instantiate_fwd(toExpr(t.tv.t))
      eval_fwd(t.tv)
      setα(ann.meet(e.__α, t.tv.v.__α), v)
   } else
   if (t instanceof Expl.MatchAs) {
      eval_fwd(t.tu)
      instantiate_fwd(toExpr(t.tv.t))
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
   if (t instanceof Expl.Var) {
      joinα(v.__α, t.v)
      return joinα(v.__α, e)
   } else
   if (t instanceof Expl.App) {
      assert(t.tf.v instanceof Closure)
      joinα(v.__α, t.tv.v)
      instantiate_bwd(eval_bwd(t.tv))
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
      instantiate_bwd(eval_bwd(t.tv))
      defs_bwd(t.def̅)
      return joinα(v.__α, e)
   } else
   if (t instanceof Expl.MatchAs) {
      joinα(v.__α, t.tv.v)
      instantiate_bwd(eval_bwd(t.tv))
      match_bwd(t.ξ, v.__α)
      eval_bwd(t.tu)
      return joinα(v.__α, e)
   } else {
      return absurd()
   }
}

}
