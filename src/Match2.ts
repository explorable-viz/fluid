import { Annotation, ann } from "./util/Annotated2"
import { Class, __nonNull, absurd, assert, className, error } from "./util/Core"
import { Pair } from "./BaseTypes2"
import { DataType, DataValue, ctrToDataType, elimSuffix } from "./DataType2"
import { Env, emptyEnv } from "./Env2"
import { Expr } from "./Expr2"
import { Str, Value, _, make } from "./Value2"
import { Versioned, asVersioned, setα } from "./Versioned2"

import Kont = Expr.Kont
import Trie = Expr.Trie

export function evalTrie<K extends Kont<K>> (σ: Trie<K>): Func<K> {
   if (Trie.Var.is(σ)) {
      return varFunc(σ)
   } else
   if (Trie.Constr.is(σ)) {
      const cases: Pair<Str, Expr.Args<K>>[] = σ.cases.toArray(),
            c̅: string[] = cases.map(({ fst: c }) => c.val),
            d: DataType = __nonNull(ctrToDataType.get(c̅[0])),
            c̅ʹ: string[] = [...d.ctrs.keys()], // also sorted
            f̅: Args.ArgsFunc<K>[] = []
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

// Parser ensures constructor calls are saturated.
function evalArgs<K extends Kont<K>> (Π: Expr.Args<K>): Args.ArgsFunc<K> {
   if (Expr.Args.End.is(Π)) {
      return Args.endFunc(Π)
   } else
   if (Expr.Args.Next.is(Π)) {
      return Args.nextFunc(Π)
   } else {
      return absurd()
   }
}

// Func to distinguish from expression-level Fun. See GitHub issue #128.
export abstract class Func<K extends Kont<K>> extends Value<"Func"> {
   abstract __apply (v: Versioned<Value>): [Env, Match<K>, K, Annotation]
}

function datatype (f: DataFunc<any>): string {
   const c: string = className(f)
   return c.substr(0, c.length - elimSuffix.length)
}

// Concrete instances have a field per constructor, in *lexicographical* order.
export abstract class DataFunc<K extends Kont<K>> extends Func<K> {
   __apply (v: Versioned<Value>): [Env, Match<K>, K, Annotation] {
      const c: string = className(v)
      if (v instanceof DataValue) {
         const d: DataType = __nonNull(ctrToDataType.get(c)),
               args_f: Args.ArgsFunc<K> = ((this as any)[c] as Args.ArgsFunc<K>)
         assert(args_f !== undefined, `Pattern mismatch: found ${c}, expected ${datatype(this)}.`)
         const v̅: Versioned<Value>[] = (v as DataValue).fieldValues().map(v => asVersioned(v)),
               [ρ, Ψ, κ, α] = args_f.__apply(v̅)
         return [ρ, make(d.matchC̅.get(c)!, v, Ψ), κ, ann.meet(v.__α, α)]
      } else {
         return error(`Pattern mismatch: ${c} is not a datatype.`, v, this)
      }
   }
}

class VarFunc<K extends Kont<K>> extends Func<K> {
   σ: Trie.Var<K> = _

   __apply (v: Versioned<Value>): [Env, Match<K>, K, Annotation] {
      return [Env.singleton(this.σ.x, v), varMatch(), this.σ.κ, ann.top]
   }
}

function varFunc<K extends Kont<K>> (σ: Trie.Var<K>): VarFunc<K> {
   return make(VarFunc, σ) as VarFunc<K>
}

export abstract class Match<K> extends Value<"Match"> {
   abstract __unapply (α: Annotation): void
}

// Concrete instances have an additional "matched args" field for the matched constructor.
export class DataMatch<K extends Kont<K>> extends Match<K> {
   v: Versioned<DataValue> = _

   __unapply (α: Annotation): void {
      const Ψ: Args.ArgsMatch<K> = __nonNull((this as any)[className(this.v)] as Args.ArgsMatch<K>)
      Ψ.__unapply(α)
      setα(α, this.v)
   }
}

class VarMatch<K extends Kont<K>> extends Match<K> {
   __unapply (α: Annotation): void {
      // nothing to do
   }
}

function varMatch<K extends Kont<K>> (): VarMatch<K> {
   return make(VarMatch)
}

export namespace Args {
   export abstract class ArgsFunc<K extends Kont<K>> extends Value<"ArgsFunc"> {
      abstract __apply (v̅: Versioned<Value>[]): [Env, ArgsMatch<K>, K, Annotation]
   }
   
   class EndFunc<K extends Kont<K>> extends ArgsFunc<K> {
      Π: Expr.Args.End<K> = _
      
      __apply (v̅: Versioned<Value>[]): [Env, ArgsMatch<K>, K, Annotation] {
         if (v̅.length === 0) {
            return [emptyEnv(), endMatch(), this.Π.κ, ann.top]
         } else {
            return absurd("Too many arguments to constructor.")
         }
      }
   }
   
   export function endFunc<K extends Kont<K>> (Π: Expr.Args.End<K>): EndFunc<K> {
      return make(EndFunc, Π) as EndFunc<K>
   }
   
   class NextFunc<K extends Kont<K>> extends ArgsFunc<K> {
      Π: Expr.Args.Next<K> = _
   
      __apply (v̅: Versioned<Value>[]): [Env, ArgsMatch<K>, K, Annotation] {
         if (v̅.length === 0) {
            return absurd("Too few arguments to constructor.")
         } else {
            const [v, ...v̅ʹ] = v̅,
                  [ρ, ξ, Π, α] = evalTrie(this.Π.σ).__apply(v),
                  [ρʹ, Ψ, κ, αʹ] = evalArgs(Π).__apply(v̅ʹ)
            return [ρ.concat(ρʹ), nextMatch(ξ, Ψ), κ, ann.meet(α, αʹ)]
         }
      }
   }
   
   export function nextFunc<K extends Kont<K>> (Π: Expr.Args.Next<K>): NextFunc<K> {
      return make(NextFunc, Π) as NextFunc<K>
   }
   
   export abstract class ArgsMatch<K> extends Value<"ArgsMatch"> {
      abstract __unapply (α: Annotation): void
   }

   class EndMatch<K extends Kont<K>> extends ArgsMatch<K> {
      __unapply (α: Annotation): void {
         // nothing to do
      }

      static is<K extends Kont<K>> (Ψ: ArgsMatch<K>): Ψ is EndMatch<K> {
         return Ψ instanceof EndMatch
      }
   }
   
   function endMatch<K extends Kont<K>> (): EndMatch<K> {
      return make(EndMatch)
   }
   
   class NextMatch<K extends Kont<K>> extends ArgsMatch<K> {
      ξ: Match<K> = _
      Ψ: ArgsMatch<K> = _
   
      __unapply (α: Annotation): void {
         if (NextMatch.is(this.Ψ)) {
            this.Ψ.Ψ.__unapply(α)
            this.ξ.__unapply(α)
         } else
         if (EndMatch.is(this.Ψ)) {
            this.ξ.__unapply(α)
         }
      }

      static is<K extends Kont<K>> (Ψ: ArgsMatch<K>): Ψ is NextMatch<K> {
         return Ψ instanceof NextMatch
      }
   }
   
   function nextMatch<K extends Kont<K>> (ξ: Match<K>, Ψ: ArgsMatch<K>): NextMatch<K> {
      return make(NextMatch, ξ, Ψ)
   }
}
