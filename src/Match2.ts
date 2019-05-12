import { __nonNull, absurd, as, assert, className, error } from "./util/Core"
import { Pair } from "./BaseTypes2"
import { DataType, ctrToDataType } from "./DataType2"
import { Func, Env } from "./Func2"
import { Eval } from "./Eval2"
import { Expr } from "./Expr2"
import { Constr, Value, _, fieldValues, make } from "./Value2"

import Args = Expr.Args
import Kont = Expr.Kont
import Trie = Expr.Trie

function interpretKont<K extends Kont<K>> (κ: K): (ρ: Env) => Value {
   if (κ instanceof Expr.Expr) {
      return Eval.interpret(κ)
   } else
   if (κ instanceof Trie.Trie) {
      return interpretTrie(κ)
   } else
   if (κ instanceof Args.Args) {
      return interpretArgs(κ)
   } else {
      return absurd()
   }
}

export function interpretTrie<K extends Kont<K>> (σ: Trie<K>): (ρ: Env) => Func {
   return (ρ: Env): Func => {
      if (Trie.Var.is(σ)) {
         return varFunc(σ, ρ)
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
         return make(d.elimC, ...f̅)
      } else {
         return absurd()
      }
   }
}

// Parser ensures constructor calls are saturated.
function interpretArgs<K extends Kont<K>> (Π: Args<K>): (ρ: Env) => ArgumentsFunc {
   return (ρ: Env): ArgumentsFunc => {
      if (Args.End.is(Π)) {
         return endFunc(Π, ρ)
      } else
      if (Args.Next.is(Π)) {
         return nextFunc(Π, ρ)
      } else {
         return absurd()
      }
   }
}

class VarFunc<K extends Kont<K>> extends Func {
   σ: Trie.Var<K> = _
   ρ: Env = _

   __apply (v: Value): Value {
      return interpretKont(this.σ.κ)(Env.concat(this.ρ, Env.singleton(this.σ.x, v)))
   }
}

function varFunc<K extends Kont<K>> (σ: Trie.Var<K>, ρ: Env): VarFunc<K> {
   return make<VarFunc<K>>(VarFunc, σ, ρ)
}

// Concrete instances must have a field per constructor, in *lexicographical* order.
export abstract class ConstrFunc extends Func {
   __apply (v: Value): Value {
      if (v instanceof Constr) {
         return ((this as any)[className(v)] as ArgumentsFunc).__apply(fieldValues(v))
      } else {
         return error(`Pattern mismatch: ${className(v)} is not a data type.`, v, this)
      }
   }
}

export abstract class ArgumentsFunc extends Value {
   abstract __apply (v̅: Value[]): Value
}

class EndFunc<K extends Kont<K>> extends ArgumentsFunc {
   Π: Args.End<K> = _
   ρ: Env = _
   
   __apply (v̅: Value[]): Value {
      if (v̅.length === 0) {
         return interpretKont(this.Π.κ)(this.ρ)
      } else {
         return absurd("Too many arguments to constructor.")
      }
   }
}

function endFunc<K extends Kont<K>> (Π: Args.End<K>, ρ: Env): EndFunc<K> {
   return make<EndFunc<K>>(EndFunc, Π, ρ)
}

class NextFunc<K extends Kont<K>> extends ArgumentsFunc {
   Π: Args.Next<K> = _
   ρ: Env = _

   __apply (v̅: Value[]): Value {
      if (v̅.length === 0) {
         return absurd("Too few arguments to constructor.")
      } else {
         const f: ArgumentsFunc = as(interpretTrie(this.Π.σ)(this.ρ).__apply(v̅[0]), ArgumentsFunc)
         return f.__apply(v̅.slice(1))
      }
   }
}

function nextFunc<K extends Kont<K>> (Π: Args.Next<K>, ρ: Env): NextFunc<K> {
   return make(NextFunc, Π, ρ)
}
