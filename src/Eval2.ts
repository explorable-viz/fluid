import { __nonNull, absurd, className, error } from "./util/Core"
import { Cons, List, Nil, nil } from "./BaseTypes2"
import { Ctr, ctrFor } from "./DataType2"
import { Closure, closure } from "./ExplVal2"
import { Expr } from "./Expr2"
import { Env, emptyEnv, extendEnv } from "./Func2"
import { interpretTrie } from "./Match2"
import { BinaryOp, binaryOps } from "./Primitive2"
import { PrimOp, Value, make, num, primOp, str } from "./Value2"

export module Eval {

// Environments are snoc-lists, so this reverses declaration order, but semantically it's irrelevant.
export function closeDefs (δ_0: List<Expr.RecDef>, ρ: Env, δ: List<Expr.RecDef>): Env {
   if (Cons.is(δ)) {
      const def: Expr.RecDef = δ.head
      return extendEnv(closeDefs(δ_0, ρ, δ.tail), def.x.str, closure(ρ, δ_0, interpretTrie(def.σ)))
   } else
   if (Nil.is(δ)) {
      return emptyEnv()
   } else {
      return absurd()
   }
}

// Repeatedly reinterprets subexpressions, so probably as slow as the previous implementation.
// Should be able to significantly speed up by memoisation.
export function interpret (e: Expr): (ρ: Env) => Value {
   return (ρ: Env): Value => {
      if (e instanceof Expr.ConstNum) {
         return num(e.val)
      } else
      if (e instanceof Expr.ConstStr) {
         return str(e.val)
      } else
      if (e instanceof Expr.Fun) {
         return closure(ρ, nil(), interpretTrie(e.σ))
      } else
      if (e instanceof Expr.Var) {
         const x: string = e.x
         if (ρ.has(x)) { 
            return ρ.get(x)!
         } else {
            return error(`Variable '${x}' not found.`)
         }
      } else
      if (e instanceof Expr.PrimOp) {
         return primOp(e.op)
      } else
      if (e instanceof Expr.App) {
         const v: Value = interpret(e.func)(ρ)
         if (v instanceof Closure) {
            const [ρʹ, eʹ]: [Env, Expr] = v.f.__apply(interpret(e.arg)(ρ)),
                  ρ_defs: Env = closeDefs(v.δ, v.ρ, v.δ)
            return interpret(eʹ)(Env.concat(v.ρ, Env.concat(ρ_defs, ρʹ)))
         } else
         // Primitives with identifiers as names are unary and first-class.
         if (v instanceof PrimOp) {
            const u: Value = interpret(e.arg)(ρ)
            return v.op.b.op(u)
         } else {
            return error(`Cannot apply a ${className(v)}`, v)
         }
      } else
      // Operators (currently all binary) are "syntax", rather than names.
      if (e instanceof Expr.BinaryApp) {
         if (binaryOps.has(e.opName.str)) {
            const op: BinaryOp = binaryOps.get(e.opName.str)!, // opName lacks annotations
                  [v1, v2]: [Value, Value] = [interpret(e.e1)(ρ), interpret(e.e2)(ρ)]
            return op.b.op(v1, v2)
         } else {
            return error("Operator name not found.", e.opName)
         }
      } else
      if (e instanceof Expr.Constr) {
         let v̅: Value[] = e.args.toArray().map((e: Expr) => interpret(e)(ρ))
         return make(ctrFor(e.ctr.str).C, ...v̅)
      } else 
      if (e instanceof Expr.Let) {
         const [ρʹ, eʹ]: [Env, Expr] = interpretTrie<Expr>(e.σ).__apply(interpret(e.e)(ρ))
         return interpret(eʹ)(Env.concat(ρ, ρʹ))
      } else
      if (e instanceof Expr.LetRec) {
         const ρʹ: Env = closeDefs(e.δ, ρ, e.δ)
         return interpret(e.e)(Env.concat(ρ, ρʹ))
      } else
      if (e instanceof Expr.MatchAs) {
         const [ρʹ, eʹ]: [Env, Expr] = interpretTrie(e.σ).__apply(interpret(e.e)(ρ))
         return interpret(eʹ)(Env.concat(ρ, ρʹ))
      } else {
         return absurd("Unimplemented expression form.", e)
      }
   }
}

}
