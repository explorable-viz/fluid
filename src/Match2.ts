import { Class, __nonNull, absurd, error } from "./util/Core"
import { Pair } from "./BaseTypes2"
import { DataType, ctrToDataType } from "./DataType2"
import { ArgumentsFunc, ConstrFunc, Func, Env, emptyEnv } from "./Func2"
import { Value, make } from "./Value2"
import { Expr } from "./Expr2"

import Args = Expr.Args
import Kont = Expr.Kont
import Trie = Expr.Trie

export function interpretTrie<K extends Kont<K>> (σ: Trie<K>): Func<K> {
   if (Trie.Var.is(σ)) {
      return new (class extends Func<K> {
         __apply (v: Value): [Env, K] {
            return [Env.singleton(σ.x, v), σ.κ]
         }
      })
   } else
   if (Trie.Constr.is(σ)) {
      const cases: Pair<string, Args<K>>[] = σ.cases.toArray(),
            c̅: string[] = cases.map(({ fst: c }) => c),
            d: DataType = __nonNull(ctrToDataType.get(c̅[0])),
            c̅ʹ: string[] = [...d.ctrs.keys()].sort(), // inefficient to do this here
            f̅: ArgumentsFunc<K>[] = []
      for (let n: number = 0; n < c̅ʹ.length; ++n) {
         if (c̅.includes(c̅ʹ[n])) {
            f̅.push(interpretArgs(cases[n].snd))
         } else {
            f̅.push(undefined as any)
         }
      }
      return make(d.elimC as Class<ConstrFunc<K>>, ...f̅)
   } else {
      return absurd()
   }
}

function interpretArgs<K extends Kont<K>> (Π: Args<K>): ArgumentsFunc<K> {
   if (Args.End.is(Π)) {
      return new (class extends ArgumentsFunc<K> {
         __apply (v̅: Value[]): [Env, K] {
            if (v̅.length === 0) {
               return [emptyEnv(), Π.κ]
            } else {
               return error("Wrong number of arguments")
            }
         }
      })
   } else
   if (Args.Next.is(Π)) {
      return new (class extends ArgumentsFunc<K> {
         __apply (v̅: Value[]): [Env, K] {
            if (v̅.length === 0) {
               return error("Wrong number of arguments")
            } else {
               const [ρ, Πʹ]: [Env, Args<K>] = interpretTrie(Π.σ).__apply(v̅[0]),
                     [ρʹ, κ]: [Env, K] = interpretArgs(Πʹ).__apply(v̅.slice(1))
               return [Env.concat(ρ, ρʹ), κ]
            }
         }
      })
   } else {
      return absurd()
   }
}
