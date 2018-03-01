import { zip } from "./util/Array"
import { __nonNull, assert, as } from "./util/Core"
import { extend, union } from "./util/Map"
import { __def, __defLocal, key, keyP } from "./Memo"
import { Env, EnvEntry, Expr, Trace, Traced, Trie, Value } from "./Syntax"

export module Eval {

type EvalResult = [Traced, Env, Traced] // v, ρ, σv

function __result (α: Addr, t: Trace, v: Value.Value | null, ρ: Env, κ: Traced): EvalResult {
   return [Traced.at(α, t, v), ρ, κ]
}

__def(eval)
export function eval_ (ρ: Env): (σ: Trie.Trie<Object> | null) => (e: Expr.Expr) => EvalResult {
   return __defLocal(key(eval, arguments), function withDemand (σ: Trie.Trie<Object>): (e: Expr.Expr) => EvalResult {
      return __defLocal(key(withDemand, arguments), function withEnv (e: Expr.Expr): EvalResult {
         const α: Addr = key(withEnv, arguments)
         assert(e !== undefined, "Missing constructor argument?")
         if (σ instanceof Trie.Var) {
            // TODO
         } else {
            if (e instanceof Expr.Constr) {               
               if (σ instanceof Trie.Constr) {
                  const σʹ: Trie.Trie<Traced> = σ.cases.get(v.ctr.str)
                  const β: Addr = keyP(α, "val")
                  return __result(α, t, Value.Constr.at(β, v.ctr, v.args.map(eval_(ρ)(null))))
               } else {
                  return assert(false, "Demand mismatch.")
               }
            } else
            if (e instanceof Expr.ConstInt) {
               return e
            } else
            if (e instanceof Expr.ConstStr) {
               return e
            } else
            if (e instanceof Expr.Fun) {
               if (σ instanceof Trie.Fun) {
                  const v: Value.Closure = Value.Closure.at(keyP(α, "val"), ρ, [], e)
                  return __result(α, Trace.Empty.at(keyP(α, "trace")), v, new Map, σ.body)
               } else {
                  assert(false, "Demand mismatch.")
               }
            } else
            // See 0.4.6 release notes on why undefined values map to ⊥.
            if (e instanceof Expr.OpName) {
               if (σ instanceof Trie.Fun) {
                  if (!ρ.has(e.opName.str)) {
                     return assert(false, "Operator not found.", e.opName)
                  } else {
                     const v: Value.PrimOp = as(ρ.get(e.opName.str), Value.PrimOp)
                     return __result(α, Trace.Empty.at(keyP(α, "trace")), v, new Map, σ.body)
                  }
               } else {
                  assert(false, "Demand mismatch.")
               }
            } else
            if (e instanceof Expr.Var) {
               if (!ρ.has(e.ident.str)) {
                  return assert(false, "Variable not found.", e.ident)
               } else {
                  const cls: EnvEntry = __nonNull(ρ.get(e.ident.str)),
                        [tv, ρʺ, σv]: EvalResult = eval_(cls.ρ)(σ)(cls.e)
                  return __result(α, Trace.Var.at(α, e.ident, tv.trace), tv.val, ρʺ, σv)
               }
            } else
            if (e instanceof Expr.Let) {
               const [tu, ρʹ, σu]: EvalResult = eval_(ρ)(e.σ)(e.e),
                     [tv, ρʺ, κ]: EvalResult = eval_(union([ρ, ρʹ]))(σ)(σu)
               return __result(α, Trace.Let.at(keyP(α, "trace"), tu, tv.trace), tv.val, ρʺ, κ)
            } else 
            // See 0.3.4 release notes for semantics.
            if (e instanceof Trace.LetRec) {
               const defs: Trace.RecDefinition[] = e.bindings.map(binding => binding.def),
                     fs: Value.Closure[] = closeDefs(ρ, defs),
                     ρʹ: Env = extend(ρ, zip(defs.map(def => def.name.str), fs)),
                     χ: EvalResult = eval_(ρʹ)(null)(Traced.at(keyP(α, "1"), t.body, null, e.val)),
                     bindings: Trace.RecBinding[] = zip(t.bindings, fs).map(bindRecDef)
               return __result(α, Trace.LetRec.at(keyP(α, "trace"), bindings, χ.expr.trace), χ.expr.val)
            } else
            if (e instanceof Expr.MatchAs) {
               const [tu, ρʹ, σu]: EvalResult = eval_(ρ)(e.σ)(e.e),
                     [tv, ρʺ, κ] : EvalResult = eval_(union([ρ, ρʹ]))(σ)(σu)
               return __result(α, Trace.Match.at(keyP(α, "trace"), tu, tv.trace), tv.val, ρʺ, κ)
            } else
            if (e instanceof Expr.App) {
               const β: Addr = keyP(α, "trace"),
                     [tf, ,]: EvalResult = eval_(ρ)(Trie.Fun.at(keyP(α, "1"), null))(e.func),
                     f: Value.Value | null = tf.val
               if (f instanceof Value.Closure) {
                  const [tu, ρ2, σʹu]: EvalResult = eval_(ρ)(f.func.σ)(e.arg),
                        // extend(, zip(f.defs.map(def => def.name.str), closeDefs(f.ρ, f.defs))
                        [tv, ρʹ, σv]: EvalResult = eval_(union([f.ρ, ρ2]))(σ)(σʹu)
                  return __result(α, Trace.App.at(β, tf, tu, tv.trace), tv.val, ρʹ, σv)
               } else
               if (f instanceof Value.PrimOp) {
                  // TODO: trie for forcing value of primitive type.
                  const [tv, ,]: EvalResult = eval_(ρ)(f.func.σ)(e.arg)                        
                  return __result(α, Trace.PrimApp.at(β, tf, tv), f._apply(tv.val), new Map, null)
               } else {
                  return assert(false, "Not an applicable value.", f)
               }
            }
            return assert(false)
         }
      })
   })
}

__def(closeDefs)
function closeDefs (ρ: Env, defs: Trace.RecDefinition[]): Value.Closure[] {
   const closeDef = 
      __defLocal(key(closeDefs, arguments), function closeDef (def: Trace.RecDefinition): Value.Closure {
         return Value.Closure.at(key(closeDef, arguments), ρ, defs, def.func)
      })
   return defs.map(closeDef)
}

__def(bindRecDef)
function bindRecDef ([binding, f]: [Trace.RecBinding, Value.Closure]): Trace.RecBinding {
  const α: Addr = key(bindRecDef, arguments)
  return Trace.RecBinding.at(α, binding.def, f)
}

}
