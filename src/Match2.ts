import { __nonNull, absurd, as, assert, className, error } from "./util/Core"
import { Pair } from "./BaseTypes2"
import { DataType, ctrToDataType } from "./DataType2"
import { Func, Env, concat, singleton } from "./Func2"
import { Eval } from "./Eval2"
import { Expr } from "./Expr2"
import { Constr, Str, Value, _, fieldValues, make } from "./Value2"

import Args = Expr.Args
import Kont = Expr.Kont
import Trie = Expr.Trie

function evalKont<K extends Kont<K>> (ρ: Env, κ: K): Value {
   if (κ instanceof Expr.Expr) {
      return Eval.eval_(ρ, κ).v // for now
   } else
   if (κ instanceof Trie.Trie) {
      return evalTrie(ρ, κ)
   } else
   if (κ instanceof Args.Args) {
      return evalArgs(ρ, κ)
   } else {
      return absurd()
   }
}

export function evalTrie<K extends Kont<K>> (ρ: Env, σ: Trie<K>): Func {
   if (Trie.Var.is(σ)) {
      return varFunc(σ, ρ)
   } else
   if (Trie.Constr.is(σ)) {
      const cases: Pair<Str, Args<K>>[] = σ.cases.toArray(),
            c̅: string[] = cases.map(({ fst: c }) => c.val),
            d: DataType = __nonNull(ctrToDataType.get(c̅[0])),
            c̅ʹ: string[] = [...d.ctrs.keys()], // also sorted
            f̅: ArgumentsFunc[] = []
      let n: number = 0
      for (let nʹ: number = 0; nʹ < c̅ʹ.length; ++nʹ) {
         if (c̅.includes(c̅ʹ[nʹ])) {
            f̅.push(evalArgs(ρ, cases[n++].snd))
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

// Parser ensures constructor calls are saturated.
function evalArgs<K extends Kont<K>> (ρ: Env, Π: Args<K>): ArgumentsFunc {
   if (Args.End.is(Π)) {
      return endFunc(Π, ρ)
   } else
   if (Args.Next.is(Π)) {
      return nextFunc(Π, ρ)
   } else {
      return absurd()
   }
}

class VarFunc<K extends Kont<K>> extends Func {
   σ: Trie.Var<K> = _
   ρ: Env = _

   __apply (v: Value): Value {
      return evalKont(concat(this.ρ, singleton(this.σ.x, v)), this.σ.κ)
   }
}

function varFunc<K extends Kont<K>> (σ: Trie.Var<K>, ρ: Env): VarFunc<K> {
   return make<VarFunc<K>>(VarFunc, σ, ρ)
}

// Concrete instances must have a field per constructor, in *lexicographical* order.
export abstract class ConstrFunc extends Func {
   __apply (v: Value): Value {
      if (v instanceof Constr) {
         return as((this as any)[className(v)], ArgumentsFunc).__apply(fieldValues(v))
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
         return evalKont(this.ρ, this.Π.κ)
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
         const f: ArgumentsFunc = as(evalTrie(this.ρ, this.Π.σ).__apply(v̅[0]), ArgumentsFunc)
         return f.__apply(v̅.slice(1))
      }
   }
}

function nextFunc<K extends Kont<K>> (Π: Args.Next<K>, ρ: Env): NextFunc<K> {
   return make(NextFunc, Π, ρ)
}
