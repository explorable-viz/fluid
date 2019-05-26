import { Annotation, ann } from "./util/Annotated2"
import { Class, __nonNull, absurd, assert, className, error } from "./util/Core"
import { List, Pair, cons, nil } from "./BaseTypes2"
import { DataValue } from "./DataValue2"
import { DataType, ctrToDataType, elimSuffix } from "./DataType2"
import { Env, emptyEnv } from "./Env2"
import { Expr } from "./Expr2"
import { Str, Value, _, make } from "./Value2"
import { Versioned, asVersioned, setα } from "./Versioned2"

import Kont = Expr.Kont
import Trie = Expr.Trie

type RuntimeKont = DataValue<"Func" | "Expr">

export function evalTrie (σ: Trie<Expr>): Func<Expr> {
   return evalTrie_(σ) as Func<Expr>
}

function evalTrie_<K extends Kont> (σ: Trie<K>): Func<RuntimeKont> {
   if (Trie.Var.is(σ)) {
      return varFunc(σ.x, evalKont(σ.κ))
   } else
   if (Trie.Constr.is(σ)) {
      const cases: Pair<Str, K>[] = σ.cases.toArray(),
            c̅: string[] = cases.map(({ fst: c }) => c.val),
            d: DataType = __nonNull(ctrToDataType.get(c̅[0])),
            c̅ʹ: string[] = [...d.ctrs.keys()], // also sorted
            f̅: RuntimeKont[] = []
      let n: number = 0
      for (let nʹ: number = 0; nʹ < c̅ʹ.length; ++nʹ) {
         if (c̅.includes(c̅ʹ[nʹ])) {
            f̅.push(evalKont(cases[n++].snd))
         } else {
            f̅.push(undefined as any)
         }
      }
      assert(n === cases.length)
      return make(d.elimC as Class<DataFunc<RuntimeKont>>, ...f̅)
   } else {
      return absurd()
   }
}

function evalKont<K extends Kont> (κ: K): RuntimeKont {
   if (κ instanceof Trie.Trie) {
      const σ: Trie<K> = κ
      return evalTrie(σ)
   } else
   if (κ instanceof Expr.Expr) {
      return κ
   } else {
      return absurd()
   }
}

// Func to distinguish from expression-level Fun. See GitHub issue #128.
export abstract class Func<K extends RuntimeKont> extends DataValue<"Func"> {
   abstract __apply (v: Versioned<Value>, ξ: Match2): [Env, Match2, K]
}

// Parser ensures constructor calls are saturated.
function __applyArgs<K extends RuntimeKont> (κ: K, v̅: Versioned<Value>[], ξ: Match2): [Env, Match2, K] {
   if (v̅.length === 0) {
      return [emptyEnv(), ξ, κ]
   } else {
      const [v, ...v̅ʹ] = v̅
      if (κ instanceof Func) {
         const f: Func<K> = κ, // "unfold" K into Func<K>
               [ρ, ξʹ, κʹ]: [Env, Match2, K] = f.__apply(v, ξ),
               [ρʹ, ξ2, κ2]: [Env, Match2, K] = __applyArgs(κʹ, v̅ʹ, ξʹ)
         return [ρ.concat(ρʹ), ξ2, κ2]
      } else {
         return absurd("Too many arguments to constructor.")
      }
   }
}

function datatype (f: DataFunc<any>): string {
   const c: string = className(f)
   return c.substr(0, c.length - elimSuffix.length)
}

// Concrete instances have a field per constructor, in *lexicographical* order.
export abstract class DataFunc<K extends RuntimeKont> extends Func<K> {
   __apply (v: Versioned<Value>, ξ: Match2): [Env, Match2, K] {
      const c: string = className(v)
      if (v instanceof DataValue) {
         const // d: DataType = __nonNull(ctrToDataType.get(c)),
               κ: K = (this as any)[c] as K
         assert(κ !== undefined, `Pattern mismatch: found ${c}, expected ${datatype(this)}.`)
         const v̅: Versioned<Value>[] = (v as DataValue).fieldValues().map(v => asVersioned(v)),
               [ρ, ξʹ, κʹ]: [Env, Match2, K] = __applyArgs(κ, v̅, ξ)
         return [ρ, cons(v, ξʹ), κʹ]
      } else {
         return error(`Pattern mismatch: ${c} is not a datatype.`, v, this)
      }
   }
}

class VarFunc<K extends RuntimeKont> extends Func<RuntimeKont> {
   x: Str = _
   κ: K = _

   __apply (v: Versioned<Value>): [Env, Match2, K] {
      return [Env.singleton(this.x, v), nil(), this.κ]
   }
}

function varFunc<K extends RuntimeKont> (x: Str, κ: K): VarFunc<K> {
   return make(VarFunc, x, κ) as VarFunc<K>
}

// Spine of matched prefix.
export type Match2 = List<Versioned<Value>>

export function match_fwd (v̅: Match2): Annotation {
   return v̅.toArray().reduce((α: Annotation, v: Versioned<Value>): Annotation => ann.meet(α, v.__α), ann.top)
}

export function match_bwd (v̅: Match2, α: Annotation) : void {
   v̅.toArray().map(v => setα(α, v))
}
