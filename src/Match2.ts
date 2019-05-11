import { Class, __nonNull, absurd, as, assert } from "./util/Core"
import { Pair } from "./BaseTypes2"
import { DataType, ctrToDataType } from "./DataType2"
import { ArgumentsFunc, ConstrFunc, Func, Env } from "./Func2"
import { Value, make } from "./Value2"
import { Eval } from "./Eval2"
import { Expr } from "./Expr2"

import Args = Expr.Args
import Kont = Expr.Kont
import Trie = Expr.Trie

function interpretKont<K extends Kont<K>> (κ: K): (ρ: Env) => Value {
   if (κ instanceof Expr.Expr) {
      return Eval.interpret(κ)
   } else
   if (κ instanceof Trie.Trie) {
      return interpretTrie(κ)
   } else {
      return absurd()
   }
}

export function interpretTrie<K extends Kont<K>> (σ: Trie<K>): (ρ: Env) => Func {
   return (ρ: Env): Func => {
      if (Trie.Var.is(σ)) {
         return new (class extends Func {
            __apply (v: Value): Value {
               return interpretKont(σ.κ)(Env.concat(ρ, Env.singleton(σ.x, v)))
            }
         })
      } else
      if (Trie.Constr.is(σ)) {
         const cases: Pair<string, Args<K>>[] = σ.cases.toArray(),
               c̅: string[] = cases.map(({ fst: c }) => c),
               d: DataType = __nonNull(ctrToDataType.get(c̅[0])),
               c̅ʹ: string[] = [...d.ctrs.keys()], // also sorted
               f̅: ArgumentsFunc[] = []
         let n: number = 0
         for (let nʹ: number = 0; nʹ < c̅ʹ.length; ++nʹ) {
            if (c̅.includes(c̅ʹ[nʹ])) {
               f̅.push(interpretArgs(cases[n++].snd)(ρ))
            } else {
               f̅.push(undefined as any)
            }
         }
         assert(n === cases.length)
         return make(d.elimC as Class<ConstrFunc>, ...f̅)
      } else {
         return absurd()
      }
   }
}

// Parser ensures that constructor calls are saturated.
function interpretArgs<K extends Kont<K>> (Π: Args<K>): (ρ: Env) => ArgumentsFunc {
   return (ρ: Env): ArgumentsFunc => {
      if (Args.End.is(Π)) {
         return new (class EndFunc extends ArgumentsFunc {
            __apply (v̅: Value[]): Value {
               if (v̅.length === 0) {
                  return interpretKont(Π.κ)(ρ)
               } else {
                  return absurd("Too many arguments to constructor.")
               }
            }
         })
      } else
      if (Args.Next.is(Π)) {
         return new (class NextFunc extends ArgumentsFunc {
            __apply (v̅: Value[]): Value {
               if (v̅.length === 0) {
                  return absurd("Too few arguments to constructor.")
               } else {
                  const f: ArgumentsFunc = as(interpretTrie(Π.σ)(ρ).__apply(v̅[0]), ArgumentsFunc)
                  return f.__apply(v̅.slice(1))
               }
            }
         })
      } else {
         return absurd()
      }
   }
}
