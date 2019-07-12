import { Annotation, ann } from "./util/Annotated"
import { Class, __nonNull, absurd, assert, className, error } from "./util/Core"
import { List, Pair, cons, nil } from "./BaseTypes"
import { DataValue } from "./DataValue"
import { DataType, ctrToDataType, elimToDataType } from "./DataType"
import { Env, emptyEnv } from "./Env"
import { Expr } from "./Expr"
import { Str, Value, _, make, memoId } from "./Value"
import { Versioned, asVersioned, setα } from "./Versioned"

import Cont = Expr.Cont
import Trie = Expr.Trie

type RuntimeCont = Expr | DataValue<"Elim">

// Conceptually (syntactic) tries map to (semantic) elim forms, and exprs map to exprs; no easy way to 
// express this in the type system.
export function evalTrie (σ: Trie<Expr>): Elim<Expr> {
   return evalTrie_(σ) as Elim<Expr>
}

function evalTrie_<K extends Cont> (σ: Trie<K>): Elim {
   if (Trie.Var.is(σ)) {
      return varElim(σ.x, evalCont(σ.κ))
   } else
   if (Trie.Constr.is(σ)) {
      const cases: Pair<Str, K>[] = σ.cases.toArray(),
            c̅: string[] = cases.map(({ fst: c }) => c.val),
            d: DataType = __nonNull(ctrToDataType.get(c̅[0])),
            c̅ʹ: string[] = [...d.ctrs.keys()], // also sorted
            f̅: RuntimeCont[] = []
      let n: number = 0
      for (let nʹ: number = 0; nʹ < c̅ʹ.length; ++nʹ) {
         if (c̅.includes(c̅ʹ[nʹ])) {
            f̅.push(evalCont(cases[n++].snd))
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

function evalCont<K extends Cont> (κ: K): RuntimeCont {
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

// Preorder traversal of all nodes in the matched prefix.
type MatchPrefix = List<Versioned<Value>>

export class Match<K> extends DataValue<"Match"> {
   ξ: MatchPrefix = _
   κ: K = _
}

// TODO: better name for this class that doesn't conflict with method 'match'.
export function match_<K extends RuntimeCont> (ξ: MatchPrefix, κ: K): Match<K> {
   return make(Match, ξ, κ) as Match<K>
}

// See GitHub issue #128.
export abstract class Elim<K extends RuntimeCont = RuntimeCont> extends DataValue<"Elim"> {
   match (v: Versioned<Value>): [Env, Match<K>] {
      return this.matchʹ(v, nil(memoId(this.match, arguments)))
   }

   abstract matchʹ (v: Versioned<Value>, ξ: MatchPrefix): [Env, Match<K>]
}

// Parser ensures constructor calls are saturated.
function matchArgs (κ: RuntimeCont, v̅: Versioned<Value>[], ξ: MatchPrefix): [Env, Match<RuntimeCont>] {
   if (v̅.length === 0) {
      return [emptyEnv(), match_(ξ, κ)]
   } else {
      const [v, ...v̅ʹ] = v̅
      if (κ instanceof Elim) {
         const f: Elim = κ, // "unfold" K into Elim<K>
               [ρ, ξκ]: [Env, Match<RuntimeCont>] = f.matchʹ(v, ξ),
               [ρʹ, ξκʹ]: [Env, Match<RuntimeCont>] = matchArgs(ξκ.κ, v̅ʹ, ξκ.ξ)
         return [ρ.concat(ρʹ), ξκʹ]
      } else {
         return absurd("Too many arguments to constructor.")
      }
   }
}

// No need to parameterise these two claseses over subtypes of RuntimeCont because only ever use them at RuntimeCont 
// itself. Concrete instances have a field per constructor, in *lexicographical* order.
export abstract class DataElim extends Elim {
   matchʹ (v: Versioned<Value>, ξ: MatchPrefix): [Env, Match<RuntimeCont>] {
      const c: string = className(v)
      if (v instanceof DataValue) {
         const κ: RuntimeCont = (this as any)[c] as RuntimeCont
         if (κ !== undefined) {
            const v̅: Versioned<Value>[] = (v as DataValue).fieldValues().map(v => asVersioned(v)),
            [ρ, ξκ]: [Env, Match<RuntimeCont>] = matchArgs(κ, v̅, ξ)
            return [ρ, match_(cons(memoId(this.matchʹ, arguments), v, ξκ.ξ), ξκ.κ)]
         } else {
            const d: DataType = elimToDataType.get(className(this))!
            if (d.ctrs.has(c)) {
               return error(`Pattern mismatch: ${c} case is undefined for ${d.name.val} eliminator.`)
            } else {
               return error(`Pattern mismatch: found ${c}, expected ${d.name.val}.`)
            }
         }
      } else {
         return error(`Pattern mismatch: ${c} is not a datatype.`, v, this)
      }
   }
}

class VarElim extends Elim {
   x: Versioned<Str> = _
   κ: RuntimeCont = _

   matchʹ (v: Versioned<Value>, ξ: MatchPrefix): [Env, Match<RuntimeCont>] {
      return [Env.singleton(this.x, v), match_(ξ, this.κ)]
   }
}

function varElim<K extends RuntimeCont> (x: Versioned<Str>, κ: RuntimeCont): VarElim {
   return make(VarElim, x, κ) as VarElim
}

export function match_fwd (ξκ: Match<Expr>): Annotation {
   return ξκ.ξ.toArray().reduce((α: Annotation, v: Versioned<Value>): Annotation => ann.meet(α, v.__α), ann.top)
}

export function match_bwd (ξκ: Match<Expr>, α: Annotation) : void {
   ξκ.ξ.toArray().map(v => setα(α, v))
}
