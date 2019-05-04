import { absurd, error } from "./util/Core"
import { map } from "./BaseTypes2"
import { Env, concat, empty, singleton } from "./Env2"
import { ArgumentsFunc, ConstrFunc, Func, Value } from "./ExplVal2"
import { Expr } from "./Expr2"

import Args = Expr.Args
import Kont = Expr.Kont
import Trie = Expr.Trie

export function interpretTrie<K extends Kont<K>> (σ: Trie<K>): Func<[Env, K]> {
   if (σ instanceof Trie.Var) {
      return {
         __apply (v: Value): [Env, K] {
            return [singleton(σ.x, v), σ.κ]
         }
      }
   } else
   if (σ instanceof Trie.Constr) {
      const f: Func<[Env, K]> = new ConstrFunc<[Env, K]>()
      map(σ.cases, ({ fst: ctr, snd: Π }): void => {
         (f as any)[ctr] = interpretArgs(Π)
      })
      return f
   } else {
      return absurd()
   }
}

function interpretArgs<K extends Kont<K>> (Π: Args<K>): ArgumentsFunc<[Env, K]> {
   if (Π instanceof Args.End) {
      return {
         __apply (v̅: Value[]): [Env, K] {
            if (v̅.length === 0) {
               return [empty(), Π.κ]
            } else {
               return error("Wrong number of arguments")
            }
         }
      }
   } else
   if (Π instanceof Args.Next) {
      return {
         __apply (v̅: Value[]): [Env, K] {
            if (v̅.length === 0) {
               return error("Wrong number of arguments")
            } else {
               const [ρ, Πʹ]: [Env, Args<K>] = interpretTrie(Π.σ).__apply(v̅[0]),
                     [ρʹ, κ]: [Env, K] = interpretArgs(Πʹ).__apply(v̅.slice(1))
               return [concat(ρ, ρʹ), κ]
            }
         }
      }
   } else {
      return absurd()
   }
}
