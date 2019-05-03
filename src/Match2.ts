import { error } from "./util/Core"
import { map } from "./BaseTypes2"
import { Env, concat, empty, singleton } from "./Env2"
import { ArgumentsFunc, Constr, Func, Func_Dyn, Value } from "./ExplVal2"
import { Expr } from "./Expr2"
import { FiniteMap } from "./FiniteMap2"

import Args = Expr.Args
import ArgsFunc = Args.ArgsFunc
import Kont = Expr.Kont
import Trie = Expr.Trie
import TrieFunc = Trie.TrieFunc

export function interpretTrie<K extends Kont<K>> (σ: Trie<K>): Func<[Env, K]> {
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
