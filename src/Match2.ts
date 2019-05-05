import { absurd, error } from "./util/Core"
import { map } from "./BaseTypes2"
import { Env, emptyEnv } from "./Env2"
import { ArgumentsFunc, ConstrFunc, Func, Value } from "./Value2"
import { Expr } from "./Expr2"

import Args = Expr.Args
import Kont = Expr.Kont
import Trie = Expr.Trie

export function interpretTrie<K extends Kont<K>> (σ: Trie<K>): Func<[Env, K]> {
   if (Trie.Var.is(σ)) {
      return {
         __apply (v: Value): [Env, K] {
            return [Env.singleton(σ.x.str, v), σ.κ]
         }
      }
   } else
   if (Trie.Constr.is(σ)) {
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
   if (Args.End.is(Π)) {
      return {
         __apply (v̅: Value[]): [Env, K] {
            if (v̅.length === 0) {
               return [emptyEnv(), Π.κ]
            } else {
               return error("Wrong number of arguments")
            }
         }
      }
   } else
   if (Args.Next.is(Π)) {
      return {
         __apply (v̅: Value[]): [Env, K] {
            if (v̅.length === 0) {
               return error("Wrong number of arguments")
            } else {
               const [ρ, Πʹ]: [Env, Args<K>] = interpretTrie(Π.σ).__apply(v̅[0]),
                     [ρʹ, κ]: [Env, K] = interpretArgs(Πʹ).__apply(v̅.slice(1))
               return [Env.concat(ρ, ρʹ), κ]
            }
         }
      }
   } else {
      return absurd()
   }
}
