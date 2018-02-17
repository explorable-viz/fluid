import { zip } from "./util/Array"
import { __nonNull, assert, as } from "./util/Core"
import { extend, union } from "./util/Map"
import { eq } from "./util/Ord"
import { __def, __defLocal, key, keyP } from "./Memo"
import { create } from "./Runtime"
import { ConstInt, Constr, ConstStr, Env, Traced, Value } from "./Syntax"
import * as AST from "./Syntax"

export module Eval {

class EvalResult<T extends Value = Value> {
   bindings: Env
   expr: Traced<T>
   demand: AST.Trie<Object>
   cont: Traced

   constructor (bindings: Env, expr: Traced<T>, demand: AST.Trie<Object>, cont: Traced) {
      this.bindings = as(bindings, Map)
      this.expr = as(expr, Traced)
      this.demand = as(demand, AST.Trie)
      this.cont = as(cont, Traced)
   }
}

function __result <T extends Value> (α: Addr, t: AST.Trace, v: T): EvalResult<T> {
   return new EvalResult(null, Traced.at(α, t, null, v), null, null)
}

__def(eval)
export function eval_ <T extends Value> (ρ: Env): (σ: AST.Trie<Object>) => (e: Traced) => EvalResult<T> {
   return __defLocal(key(eval, arguments), function withDemand (σ: AST.Trie<Object>): (e: Traced) => EvalResult<T> {
      return __defLocal(key(withDemand, arguments), function withEnv (e: Traced): EvalResult<T> {
         const α: Addr = key(withEnv, arguments)
         assert(e !== undefined, "Missing constructor argument?")
         // TODO: case where demand is empty.
         // TODO: variable trie.
         return e.trace.__visit({
            is_EmptyTrace (t: AST.EmptyTrace): EvalResult<T> {
               return assert(false)
/*               
               return Traced.at(α, t, null, __nonNull(e.val).__visit({
                  is_Constr (v_: Constr): Object {
                     const β: Addr = keyP(α, 'val')
                     return Constr.at_(β, v_.ctr, map(v_.args, eval_(ρ)(null)))
                  },

                  is_ConstInt (_: ConstInt): Int {
                     return <Int>e.val
                  },

                  is_ConstStr (_: ConstStr): String {
                     return <String>e.val
                  }
               }))
*/
            },

            is_Fun (t: AST.Fun): EvalResult<AST.Closure> {
               return __result(α, t, AST.Closure.at(keyP(α, "val"), ρ, [], t))
            },

            // See 0.4.6 release notes on why undefined values map to ⊥.
            is_OpName (t: AST.OpName): EvalResult<AST.PrimOp> {
               assert(e.val === null)
               const v_opt: Value | undefined = ρ.get(t.opName.str)
               if (v_opt === undefined) {
                  return assert(false, "Operator not found.", t)
               } else {
                  return __result(α, t, as(v_opt, AST.PrimOp))
               }
            },

            is_Var (t: AST.Var): EvalResult<Value> {
               assert(e.val === null)
               return __result(α, t, ρ.get(t.ident.str))
            },

            is_Let (t: AST.Let): EvalResult<Value> {
               const χ: EvalResult<Value> = eval_(ρ)(t.σ)(t.e),
                     χʹ: EvalResult<Value> = eval_(union([ρ, χ.bindings]))(σ)(χ.cont)
               return __result(
                  α, 
                  AST.Let.at(keyP(α, "trace"), χ.expr, χ.demand as AST.VarTrie<Value>), 
                  χʹ.cont.val
               )
            },

            // See 0.3.4 release notes for semantics.
            is_LetRec (t: AST.LetRec): EvalResult<Object> {
               const defs: AST.RecDefinition[] = t.bindings.map(binding => binding.def),
                     fs: AST.Closure[] = closeDefs(ρ, defs),
                     ρ_: Env = extend(ρ, zip(map(defs, name), fs)),
                     χ: EvalResult<Object> = eval_(ρ_)(null)(__tracedK(keyP(α, '1'), t.body, e.val)),
                     bindings: List<AST.RecBinding> = map(zip(t.bindings, fs), bindRecDef)
               return __result(α, AST.LetRec.at_(keyP(α, 'expr', 't'), bindings, χ.expr.trace), χ.expr.val)
            },

/*
            is_App (t: AST.App): EvalResult<Object> {
               const β: Addr = keyP(α, 'expr', 't'),
                     γ: Addr = keyP(β, 'appBody', 'v'),
                     χ: EvalResult<Object> = eval_(ρ)(AST.FunTrie.at_(keyP(α, '1'), Unit.at(keyP(α, '2'))))(t.func),
                     f: Object = χ.expr.val
               if (f instanceof AST.Closure) {
                  const χʹ: EvalResult<Object> = eval_(ρ)(f.func.σ)(t.arg),
                        ρʹ: Env = extend(f.ρ, zip(map(f.defs, name), closeDefs(f.ρ, f.defs))),
                        χʺ: EvalResult<Object> = eval_(union(ρʹ, χʹ.bindings))(σ)(χʹ.cont)
                  return __result(
                           α,
                           AST.App.at_(β, χ.expr, χʹ.expr, AST.FunBody.at_(γ, χʺ.demand)),
                           χʺ.cont.val
                        )
               } else
               if (f instanceof AST.PrimOp) {
                  const χʹ: EvalResult<Object> = eval_(ρ)(null)(t.arg)
                  // Treat a primitive (which is always unary) as having an anonymous formal parameter.
                  // TODO: trie for forcing value of primitive type.
                  return __result(
                     α,
                     AST.App.at_(β, χ.expr, χʹ.expr, AST.PrimBody.at_(γ, Str.at(keyP(γ, 'param', 'v'), '_'))),
                     f.__apply(χʹ.expr.val)
                  )
               } else {
                  return assertMessage(false, 'Not an applicable value.', χ.expr)
               }
            },

            is_MatchAs (t: AST.MatchAs): EvalResult<Object> {
               const χ: EvalResult<Object> = eval_(ρ)(t.σ)(t.e),
                     χʹ: EvalResult<Object> = eval_(union(ρ, χ.bindings))(σ)(χ.cont)
               return __result(α, AST.MatchAs.at_(keyP(α, 'expr', 't'), χ.expr, χ.demand), χʹ.cont.val)
            }
*/
         })
      })
   })
}

__def(closeDefs)
function closeDefs (ρ: Env, defs: AST.RecDefinition[]): AST.Closure[] {
   const closeDef = 
      __defLocal(key(closeDefs, arguments), function closeDef (def: AST.RecDefinition): AST.Closure {
         return AST.Closure.at(key(closeDef, arguments), ρ, defs, def.func)
      })
   return defs.map(closeDef)
}

__def(bindRecDef)
function bindRecDef ([binding, f]: [AST.RecBinding, AST.Closure]): AST.RecBinding {
  const α: Addr = key(bindRecDef, arguments)
  return AST.RecBinding.at(α, binding.def, f)
}

// Return matched pattern and modified environment iff there is a successful match.
// Also see 0.6.4 release notes.
__def(match)
function match (v: Traced, ρ: Env, p: Traced): [Traced, Env] | null {
   const α: Addr = key(match, arguments)
   if (p.name !== null) {
      // variable pattern always succeeds (check whether I need to clone var here)
      assert(p.val === null)
      return [Traced.at(α, v.trace, p.name, v.val), extend(ρ, [[p.name.str, v.val]])]
   } else {
      // otherwise succeed iff constructors match and sub-patterns match sub-values
      const v_: AST.Constr = as(v.val, AST.Constr),
            p_: AST.Constr = as(p.val, AST.Constr)
      if (eq(__nonNull(v_).ctr, p_.ctr)) {
         let matched: boolean = true
         const submatch =
            __defLocal(α, function submatch ([v, p]: [Traced, Traced]): Traced {
               const vρ_opt: [Traced, Env] | null = match(v, ρ, p)
               if (vρ_opt === null) {
                  matched = false
                  return p
               } else {
                  ρ = vρ_opt[1]
                  return vρ_opt[0]
               }
            })
         const ps: Traced[] = zip(v_.args, p_.args).map(submatch)
         if (matched) {
            const v_: AST.Constr = AST.Constr.at(keyP(α, "v_"), p_.ctr, ps)
            return [Traced.at(α, v.trace, null, v_), ρ]
         }
      }
      return null
   }
}

}
