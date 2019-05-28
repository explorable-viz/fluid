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

type RuntimeKont = Expr | DataValue<"Elim">

// Conceptually (syntactic) tries map to (semantic) elim forms, and exprs map to exprs; no easy way to 
// express this in the type system.
export function evalTrie (σ: Trie<Expr>): Elim<Expr> {
   return evalTrie_(σ) as Elim<Expr>
}

function evalTrie_<K extends Kont> (σ: Trie<K>): Elim {
   if (Trie.Var.is(σ)) {
      return varElim(σ.x, evalKont(σ.κ))
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
      return make(d.elimC as Class<DataElim>, ...f̅)
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

// Preorder traversal of all nodes in the matched prefix, 
export type Match = List<Versioned<Value>>

// See GitHub issue #128.
export abstract class Elim<K extends RuntimeKont = RuntimeKont> extends DataValue<"Elim"> {
   abstract __apply (v: Versioned<Value>, ξ: Match): [Env, Match, K]
}

// Parser ensures constructor calls are saturated.
function __applyArgs (κ: RuntimeKont, v̅: Versioned<Value>[], ξ: Match): [Env, Match, RuntimeKont] {
   if (v̅.length === 0) {
      return [emptyEnv(), ξ, κ]
   } else {
      const [v, ...v̅ʹ] = v̅
      if (κ instanceof Elim) {
         const f: Elim = κ, // "unfold" K into Elim<K>
               [ρ, ξʹ, κʹ]: [Env, Match, RuntimeKont] = f.__apply(v, ξ),
               [ρʹ, ξ2, κ2]: [Env, Match, RuntimeKont] = __applyArgs(κʹ, v̅ʹ, ξʹ)
         return [ρ.concat(ρʹ), ξ2, κ2]
      } else {
         return absurd("Too many arguments to constructor.")
      }
   }
}

function datatype (f: DataElim): string {
   const c: string = className(f)
   return c.substr(0, c.length - elimSuffix.length)
}

// No need to parameterise these two claseses over subtypes of RuntimeKont because only ever use them at RuntimeKont 
// itself. Concrete instances have a field per constructor, in *lexicographical* order.
export abstract class DataElim extends Elim {
   __apply (v: Versioned<Value>, ξ: Match): [Env, Match, RuntimeKont] {
      const c: string = className(v)
      if (v instanceof DataValue) {
         const κ: RuntimeKont = (this as any)[c] as RuntimeKont
         assert(κ !== undefined, `Pattern mismatch: found ${c}, expected ${datatype(this)}.`)
         const v̅: Versioned<Value>[] = (v as DataValue).fieldValues().map(v => asVersioned(v)),
               [ρ, ξʹ, κʹ]: [Env, Match, RuntimeKont] = __applyArgs(κ, v̅, ξ)
         return [ρ, cons(v, ξʹ), κʹ]
      } else {
         return error(`Pattern mismatch: ${c} is not a datatype.`, v, this)
      }
   }
}

class VarElim extends Elim {
   x: Str = _
   κ: RuntimeKont = _

   __apply (v: Versioned<Value>): [Env, Match, RuntimeKont] {
      return [Env.singleton(this.x, v), nil(), this.κ]
   }
}

function varElim<K extends RuntimeKont> (x: Str, κ: RuntimeKont): VarElim {
   return make(VarElim, x, κ) as VarElim
}

export function match_fwd (v̅: Match): Annotation {
   return v̅.toArray().reduce((α: Annotation, v: Versioned<Value>): Annotation => ann.meet(α, v.__α), ann.top)
}

export function match_bwd (v̅: Match, α: Annotation) : void {
   v̅.toArray().map(v => setα(α, v))
}
