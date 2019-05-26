import { Annotation, ann } from "./util/Annotated2"
import { Class, __nonNull, absurd, assert, className, error, notYetImplemented } from "./util/Core"
import { List, Pair } from "./BaseTypes2"
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
   abstract __apply (v: Versioned<Value>): [Env, Match, K]
}

// Parser ensures constructor calls are saturated.
function __applyArgs<K extends RuntimeKont> (κ: K, v̅: Versioned<Value>[]): [Env, Match, K] {
   if (v̅.length === 0) {
      return [emptyEnv(), dummyMatch(), κ]
   } else {
      const [v, ...v̅ʹ] = v̅
      if (κ instanceof Func) {
         const [ρ, /*ξ*/, κʹ] = κ.__apply(v),
               [ρʹ, /*Ψ*/, κ2] = __applyArgs(κʹ, v̅ʹ)
         return [ρ.concat(ρʹ), dummyMatch()/*nextMatch(ξ, Ψ)*/, κ2]
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
   __apply (v: Versioned<Value>): [Env, Match, K] {
      const c: string = className(v)
      if (v instanceof DataValue) {
         const // d: DataType = __nonNull(ctrToDataType.get(c)),
               κ: K = (this as any)[c] as K
         assert(κ !== undefined, `Pattern mismatch: found ${c}, expected ${datatype(this)}.`)
         const v̅: Versioned<Value>[] = (v as DataValue).fieldValues().map(v => asVersioned(v))
         return __applyArgs(κ, v̅)
      } else {
         return error(`Pattern mismatch: ${c} is not a datatype.`, v, this)
      }
   }
}

class VarFunc<K extends RuntimeKont> extends Func<RuntimeKont> {
   x: Str = _
   κ: K = _

   __apply (v: Versioned<Value>): [Env, Match, K] {
      return [Env.singleton(this.x, v), dummyMatch()/*varMatch()*/, this.κ]
   }
}

function varFunc<K extends RuntimeKont> (x: Str, κ: K): VarFunc<K> {
   return make(VarFunc, x, κ) as VarFunc<K>
}

// Spine of matched prefix.
export type Match2 = List<Versioned<Value>>

export function match__fwd (v̅: Match2): Annotation {
   return v̅.toArray().reduce((α: Annotation, v: Versioned<Value>): Annotation => ann.meet(α, v.__α), ann.top)
}

export function match__bwd (v̅: Match2, α: Annotation) : void {
   v̅.toArray().map(v => setα(α, v))
}

export abstract class Match extends Value<"Match"> {
   abstract __fwd (): Annotation
   abstract __bwd (α: Annotation): void
}

class DummyMatch extends Match {
   __fwd (): Annotation {
      return ann.top
   }
   __bwd (α: Annotation): void {
   }
}

function dummyMatch (): DummyMatch {
   return make(DummyMatch)
}

// Concrete instances have an additional "matched args" field for the matched constructor.
export class DataMatch extends Match {
   v: Versioned<DataValue> = _

   __fwd (): Annotation {
      return notYetImplemented()
/*
      const Ψ: Args.ArgsMatch = (this as any)[className(this.v)] as Args.ArgsMatch
      return ann.meet(this.v.__α, Ψ.__fwd())
*/
   }

   __bwd (α: Annotation): void {
      return notYetImplemented()
/*
      const Ψ: Args.ArgsMatch = __nonNull((this as any)[className(this.v)] as Args.ArgsMatch)
      Ψ.__bwd(α)
      setα(α, this.v)
*/
   }
}

/*
class VarMatch extends Match {
   __fwd (): Annotation {
      return ann.top
   }

   __bwd (α: Annotation): void {
      // nothing to do
   }
}

function varMatch<K extends Kont<K>> (): VarMatch {
   return make(VarMatch)
}

namespace Args {
   export abstract class ArgsFunc<K extends Kont<K>> extends Value<"ArgsFunc"> {
      abstract __apply (v̅: Versioned<Value>[]): [Env, ArgsMatch, K]
   }
   
   class EndFunc<K extends Kont<K>> extends ArgsFunc<K> {
      κ: K = _
      
      __apply (v̅: Versioned<Value>[]): [Env, ArgsMatch, K] {
         if (v̅.length === 0) {
            return [emptyEnv(), endMatch(), this.κ]
         } else {
            return absurd("Too many arguments to constructor.")
         }
      }
   }
   
   export function endFunc<K extends Kont<K>> (κ: K): EndFunc<K> {
      return make(EndFunc, κ) as EndFunc<K>
   }
   
   class NextFunc<K extends Kont<K>> extends ArgsFunc<K> {
      Π: Func<Expr.Args.Args<K>> = _
   
      __apply (v̅: Versioned<Value>[]): [Env, ArgsMatch, K] {
         if (v̅.length === 0) {
            return absurd("Too few arguments to constructor.")
         } else {
            const [v, ...v̅ʹ] = v̅,
                  [ρ, ξ, Πʹ] = this.Π.__apply(v),
                  [ρʹ, Ψ, κ] = evalArgs(Πʹ).__apply(v̅ʹ)
            return [ρ.concat(ρʹ), nextMatch(ξ, Ψ), κ]
         }
      }
   }
   
   export function nextFunc<K extends Kont<K>> (Π: Func<Expr.Args.Args<K>>): NextFunc<K> {
      return make(NextFunc, Π) as NextFunc<K>
   }
   
   export abstract class ArgsMatch extends Value<"ArgsMatch"> {
      abstract __fwd (): Annotation
      abstract __bwd (α: Annotation): void
   }

   class EndMatch extends ArgsMatch {
      __fwd (): Annotation {
         return ann.top
      }

      __bwd (α: Annotation): void {
         // nothing to do
      }
   }
   
   function endMatch (): EndMatch {
      return make(EndMatch)
   }
   
   class NextMatch extends ArgsMatch {
      ξ: Match = _
      Ψ: ArgsMatch = _

      __fwd (): Annotation {
         return ann.meet(this.ξ.__fwd(), this.Ψ.__fwd())
      }

      __bwd (α: Annotation): void {
         if (this.Ψ instanceof NextMatch) {
            this.Ψ.Ψ.__bwd(α)
         }
         this.ξ.__bwd(α)
      }
   }
   
   function nextMatch (ξ: Match, Ψ: ArgsMatch): NextMatch {
      return make(NextMatch, ξ, Ψ)
   }
}
*/
