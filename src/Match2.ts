import { Annotation, ann } from "./util/Annotated2"
import { Class, __nonNull, absurd, assert } from "./util/Core"
import { Pair } from "./BaseTypes2"
import { DataType, ctrToDataType } from "./DataType2"
import { Env, emptyEnv } from "./Env2"
import { ArgsFunc, DataFunc, Func } from "./Func2"
import { Expr } from "./Expr2"
import { Str, Value, _, make } from "./Value2"
import { Versioned } from "./Versioned2"

import Args = Expr.Args
import Kont = Expr.Kont
import Trie = Expr.Trie

export function evalTrie<K extends Kont<K>> (σ: Trie<K>): Func<K> {
   if (Trie.Var.is(σ)) {
      return varFunc(σ)
   } else
   if (Trie.Constr.is(σ)) {
      const cases: Pair<Str, Args<K>>[] = σ.cases.toArray(),
            c̅: string[] = cases.map(({ fst: c }) => c.val),
            d: DataType = __nonNull(ctrToDataType.get(c̅[0])),
            c̅ʹ: string[] = [...d.ctrs.keys()], // also sorted
            f̅: ArgsFunc<K>[] = []
      let n: number = 0
      for (let nʹ: number = 0; nʹ < c̅ʹ.length; ++nʹ) {
         if (c̅.includes(c̅ʹ[nʹ])) {
            f̅.push(evalArgs(cases[n++].snd))
         } else {
            f̅.push(undefined as any)
         }
      }
      assert(n === cases.length)
      return make(d.elimC as Class<DataFunc<K>>, ...f̅)
   } else {
      return absurd()
   }
}

// TODO: sync up with evalTrie/__apply pattern.
export function unmatch<K extends Kont<K>> (κ: K, α: Annotation): void {
}

// Parser ensures constructor calls are saturated.
function evalArgs<K extends Kont<K>> (Π: Args<K>): ArgsFunc<K> {
   if (Args.End.is(Π)) {
      return endFunc(Π)
   } else
   if (Args.Next.is(Π)) {
      return nextFunc(Π)
   } else {
      return absurd()
   }
}

class VarFunc<K extends Kont<K>> extends Func<K> {
   σ: Trie.Var<K> = _

   __apply (v: Versioned<Value>): [Env, K, Annotation] {
      return [Env.singleton(this.σ.x, v), this.σ.κ, ann.top]
   }
}

function varFunc<K extends Kont<K>> (σ: Trie.Var<K>): VarFunc<K> {
   return make(VarFunc, σ) as VarFunc<K>
}

class EndFunc<K extends Kont<K>> extends ArgsFunc<K> {
   Π: Args.End<K> = _
   
   __apply (v̅: Versioned<Value>[]): [Env, K, Annotation] {
      if (v̅.length === 0) {
         return [emptyEnv(), this.Π.κ, ann.top]
      } else {
         return absurd("Too many arguments to constructor.")
      }
   }
}

function endFunc<K extends Kont<K>> (Π: Args.End<K>): EndFunc<K> {
   return make(EndFunc, Π) as EndFunc<K>
}

class NextFunc<K extends Kont<K>> extends ArgsFunc<K> {
   Π: Args.Next<K> = _

   __apply (v̅: Versioned<Value>[]): [Env, K, Annotation] {
      if (v̅.length === 0) {
         return absurd("Too few arguments to constructor.")
      } else {
         const [v, ...v̅ʹ] = v̅,
               [ρ, Π, α]: [Env, Args<K>, Annotation] = evalTrie(this.Π.σ).__apply(v),
               [ρʹ, κ, αʹ]: [Env, K, Annotation] = evalArgs(Π).__apply(v̅ʹ)
         return [ρ.concat(ρʹ), κ, ann.meet(α, αʹ)]
      }
   }
}

function nextFunc<K extends Kont<K>> (Π: Args.Next<K>): NextFunc<K> {
   return make(NextFunc, Π) as NextFunc<K>
}
