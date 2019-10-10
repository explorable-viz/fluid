import { zip, zipWith } from "./util/Array"
import { Class, __nonNull, absurd, assert, className, error } from "./util/Core"
import { eq } from "./util/Ord"
import { Annotation, ann } from "./util/Lattice"
import { setjoinα } from "./Annotated"
import { List, Pair, cons, nil } from "./BaseTypes"
import { DataValue, ExplValue } from "./DataValue"
import { DataType, ctrToDataType, elimToDataType } from "./DataType"
import { Env, emptyEnv } from "./Env"
import { Expl } from "./Expl"
import { Expr } from "./Expr"
import { Str, Value, _, fields, make } from "./Value"
import { ν } from "./Versioned"

import Cont = Expr.Cont
import Trie = Expr.Trie

// Conceptually (syntactic) tries map to (semantic) elim forms, and exprs map to exprs; no easy way to 
// express this in the type system.
export function evalTrie (σ: Trie<Expr>): Elim<Expr> {
   return evalTrie_(σ) as Elim<Expr>
}

// cκ̅ non-empty and constructors all of the same datatype.
export function constrElim<K extends Cont> (...cκ̅: [string, K][]): Elim<K> {
   const d: DataType = __nonNull(ctrToDataType.get(cκ̅[0][0])),
         c̅: string[] = cκ̅.map((([c, _]) => c)),
         c̅ʹ: string[] = [...d.ctrs.keys()], // sorted
         f̅: Cont[] = []
   let n: number = 0
   for (let nʹ: number = 0; nʹ < c̅ʹ.length; ++nʹ) {
      if (c̅.includes(c̅ʹ[nʹ])) {
         f̅.push(cκ̅[n++][1])
      } else {
         f̅.push(undefined as any)
      }
   }
   return make(d.elimC as Class<DataElim<K>>, ...f̅)
}

// Unrelated to the annotation lattice. Expr case intentionally only defined for higher-order (function) case.
function join<K extends Cont> (κ: K, κʹ: K): K {
   if (κ instanceof Trie.Trie && κʹ instanceof Trie.Trie) {
      return Trie.Trie.join<K>(κ, κʹ) as K
   } else
   if (κ instanceof Expr.Fun && κʹ instanceof Expr.Fun) {
      return Expr.fun(join(κ.σ, κʹ.σ))(ν()) as Expr as K
   } else {
      return absurd("Undefined join.", κ, κʹ)
   }
}

export function elimJoin<K extends Cont> (σ: Elim<K>, τ: Elim<K>): Elim<K> {
   if (VarElim.is(σ) && VarElim.is(τ) && eq(σ.x, τ.x)) {
      return varElim(σ.x, join(σ.κ, τ.κ))
   } else
   if (DataElim.is(σ) && DataElim.is(τ)) {
      // Both maps (which are non-empty) can (inductively) be assumed to have keys taken from the 
      // same datatype. Ensure that invariant is preserved:
      const c_σ: string = fields(σ)[0],
            c_τ: string = fields(τ)[0]
      if (ctrToDataType.get(c_σ) !== ctrToDataType.get(c_τ)) {
         error(`${c_σ} and ${c_τ} are constructors of different datatypes.`)
      }
      const cκ̅1: [string, K][] = zip(fields(σ), σ.__children as K[]),
            cκ̅2: [string, K][] = zip(fields(τ), τ.__children as K[])
      assert(cκ̅1.length === cκ̅2.length)
      const cκ̅: [string, K][] = zipWith(([c1, κ1]: [string, K], [c2, κ2]: [string, K]): [string, K] => {
         assert(c1 === c2)
         return [c1, κ1 === undefined ? κ2 : (κ2 === undefined ? κ1 : join(κ1, κ2))]
      }
      )(cκ̅1, cκ̅2)
      return constrElim(...cκ̅)
   } else {
      return absurd("Undefined join.", σ, τ)
   }
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
            f̅: Cont[] = []
      let n: number = 0
      for (let nʹ: number = 0; nʹ < c̅ʹ.length; ++nʹ) {
         if (c̅.includes(c̅ʹ[nʹ])) {
            f̅.push(evalCont(cases[n++].snd))
         } else {
            f̅.push(undefined as any)
         }
      }
      assert(n === cases.length)
      return make(d.elimC as Class<DataElim<K>>, ...f̅)
   } else {
      return absurd()
   }
}

function evalCont<K extends Cont> (κ: K): Cont {
   if (κ instanceof Trie.Trie) {
      const σ: Trie<K> = κ
      return evalTrie(σ) as any // hack for now; delete soon
   } else
   if (κ instanceof Expr.Expr) {
      return κ
   } else {
      return absurd()
   }
}

// Preorder traversal of all nodes in the matched prefix.
type MatchPrefix = List<ExplValue>

export class Match<K> extends DataValue<"Match"> {
   tv̅: MatchPrefix = _
   κ: K = _
}

export function match<K extends Cont> (ξ: MatchPrefix, κ: K): Match<K> {
   return make(Match, ξ, κ) as Match<K>
}

// See GitHub issue #128.
export abstract class Elim<K extends Cont = Cont> extends DataValue<"Elim"> {
   // could have called this "match", but conflicts with factory method of same name
   apply (tv: ExplValue): [Env, Match<K>] {
      return this.apply_(tv, nil())
   }

   abstract apply_ (tv: ExplValue, ξ: MatchPrefix): [Env, Match<K>]
}

// Parser ensures constructor calls are saturated.
function matchArgs<K extends Cont> (κ: K, tv̅: ExplValue[], u̅: MatchPrefix): [Env, Match<K>] {
   if (tv̅.length === 0) {
      return [emptyEnv(), match(u̅, κ)]
   } else {
      const [tv, ...tv̅ʹ] = tv̅
      if (κ instanceof Elim) {
         const f: Elim<K> = κ, // "unfold" K into Elim<K>
               [ρ, ξ]: [Env, Match<K>] = f.apply_(tv, u̅),
               [ρʹ, ξʹ]: [Env, Match<K>] = matchArgs(ξ.κ, tv̅ʹ, ξ.tv̅)
         return [ρ.concat(ρʹ), ξʹ]
      } else {
         return absurd("Too many arguments to constructor.")
      }
   }
}

// Concrete instances have a field per constructor, in *lexicographical* order.
export abstract class DataElim<K extends Cont = Cont> extends Elim<K> {
   static is<K extends Cont> (σ: Elim<K>): σ is DataElim<K> {
      return σ instanceof DataElim
   }

   apply_ (tv: ExplValue, u̅: MatchPrefix): [Env, Match<K>] {
      const v: Value = tv.v,
            c: string = className(v)
      if (v instanceof DataValue) {
         const κ: K = (this as any)[c] as K
         if (κ !== undefined) {
            const tv̅: ExplValue[] = Expl.explChildren(tv.t, v),
            [ρ, ξ]: [Env, Match<K>] = matchArgs(κ, tv̅, u̅)
            return [ρ, match(cons(tv, ξ.tv̅), ξ.κ)]
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

export class VarElim<K extends Cont> extends Elim<K> {
   x: Str = _
   κ: K = _

   static is<K extends Cont> (σ: Elim<K>): σ is VarElim<K> {
      return σ instanceof VarElim
   }

   apply_ (tv: ExplValue, ξ: MatchPrefix): [Env, Match<K>] {
      return [Env.singleton(this.x, tv), match(ξ, this.κ)]
   }
}

export function varElim<K extends Cont> (x: Str, κ: K): VarElim<K> {
   return make(VarElim, x, κ) as VarElim<K>
}

export function apply_fwd (ξ: Match<Expr>): Annotation {
   return ξ.tv̅.toArray().reduce((α: Annotation, tv: ExplValue): Annotation => ann.meet(α, tv.t.__α), ann.top)
}

export function apply_bwd (ξ: Match<Expr>, α: Annotation): void {
   ξ.tv̅.toArray().map((tv: ExplValue): Value => setjoinα(α, tv.t))
}
