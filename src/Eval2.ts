import { __nonNull, absurd, error } from "./util/Core"
import { List, ListFunc, map } from "./BaseTypes2"
import { DataType, datatypeFor } from "./DataType2"
import { Env, concat, empty, singleton } from "./Env2"
import { Expr } from "./Expr2"
import { ArgumentsFunc, Constr, Func, Func_Dyn, State_Dyn, Value, construct } from "./ExplVal2"
import { FiniteMap } from "./FiniteMap2"

import Args = Expr.Args
import ArgsFunc = Args.ArgsFunc
import ExprFunc = Expr.ExprFunc
import Kont = Expr.Kont
import Trie = Expr.Trie
import TrieFunc = Trie.TrieFunc

type InterpretExpr = (ρ: Env) => Value

// Repeatedly reinterprets subexpressions, so probably as slow as the previous implementation.
// Should be able to significantly speed up by memoisation.
export function interpret (e: Expr): InterpretExpr {
   return e.__match(new (class extends ExprFunc<InterpretExpr> {
      Var (x: string): InterpretExpr {
         return (ρ: Env) => __nonNull(ρ[x])
      }
      Constr(ctr: string, args: List<Expr>): InterpretExpr {
         return (ρ: Env): Value => {
            const d: DataType = __nonNull(datatypeFor.get(ctr)),
                  state: State_Dyn = {}
            let e̅: List<Expr> = args
            for (const f of d.fields) {
               e̅.__match(new (class extends ListFunc<Expr, void> {
                  Nil (): void {
                     absurd()
                  }
                  Cons (e: Expr, e̅ʹ: List<Expr>): void {
                     state[f] = interpret(e)(ρ)
                     e̅ = e̅ʹ
                  }
               }))
            }
            return construct(new d.cls, state)
         }
      }
      Fun (σ: Trie<Expr>): InterpretExpr {
         return (ρ: Env) => interpretTrie(σ)
      }
      MatchAs (e: Expr, σ: Trie<Expr>): InterpretExpr {
         return (ρ: Env): Value => {
            const [ρʹ, eʹ] = interpretTrie(σ).__apply(interpret(e)(ρ))
            return interpret(eʹ)(concat(ρ, ρʹ))
         }
      }
   }))
}

function interpretTrie<K extends Kont<K>> (σ: Trie<K>): Func<[Env, K]> {
   return σ.__match(new (class extends TrieFunc<K, Func<[Env, K]>> {
      Var (x: string, κ: K): Func<[Env, K]> {
         return {
            __apply (v: Value): [Env, K] {
               return [singleton(x, v), κ]
            }
         }
      }
      Constr (cases: FiniteMap<string, Args<K>>): Func<[Env, K]> {
         const handlers: Func<[Env, K]> = {
            __apply (v: Value): [Env, K] {
               if (v instanceof Constr) {
                  return v.__match(this)
               } else {
                  return error("Not a datatype")
               }
            }
         }
         const handlers_dyn: Func_Dyn<[Env, K]> = handlers as any // urgh
         map(cases, ({ fst: ctr, snd: Π }): void => {
            handlers_dyn[ctr] = interpretArgs(Π)
         })
         return handlers
      }
   }))
}

function interpretArgs<K extends Kont<K>> (Π: Args<K>): ArgumentsFunc<[Env, K]> {
   return Π.__match(new (class extends ArgsFunc<K, Func<[Env, K]>> {
      End (κ: K): ArgumentsFunc<[Env, K]> {
         return {
            __apply (v̅: Value[]): [Env, K] {
               if (v̅.length === 0) {
                  return [empty(), κ]
               } else {
                  return error("Wrong number of arguments")
               }
            }
         }
      }
      Next (σ: Trie<Args<K>>): Func<[Env, K]> {
         return {
            __apply (v̅: Value[]): [Env, K] {
               if (v̅.length === 0) {
                  return error("Wrong number of arguments")
               } else {
                  const [ρ, Π]: [Env, Args<K>] = interpretTrie(σ).__apply(v̅[0]),
                        [ρʹ, κ]: [Env, K] = interpretArgs(Π).__apply(v̅.slice(1))
                  return [concat(ρ, ρʹ), κ]
               }
            }
         }
      }
   }))
}
