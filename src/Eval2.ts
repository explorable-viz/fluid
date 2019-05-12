import { __nonNull, absurd, className, error } from "./util/Core"
import { ctrFor } from "./DataType2"
import { Expr } from "./Expr2"
import { Env, Func } from "./Func2"
import { interpretTrie } from "./Match2"
import { BinaryOp, binaryOps } from "./Primitive2"
import { PrimOp, Value, make, num, primOp, str } from "./Value2"

export module Eval {

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
         return interpretTrie(e.σ)(ρ)
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
         if (v instanceof Func) {
            return v.__apply(interpret(e.arg)(ρ))
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
         return make(ctrFor(e.ctr).C, ...v̅)
      } else 
      if (e instanceof Expr.Let) {
         return interpretTrie(e.σ)(ρ).__apply(interpret(e.e)(ρ))
      } else
      if (e instanceof Expr.LetRec) {
         return interpret(e.e)(ρ)
      } else
      if (e instanceof Expr.MatchAs) {
         return interpretTrie(e.σ)(ρ).__apply(interpret(e.e)(ρ))
      } else {
         return absurd("Unimplemented expression form.", e)
      }
   }
}

}
