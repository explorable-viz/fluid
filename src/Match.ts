import { zip, zipWith } from "./util/Array"
import { Class, __nonNull, absurd, assert, className, error } from "./util/Core"
import { eq } from "./util/Ord"
import { Annotation, ann } from "./util/Lattice"
import { setjoinα } from "./Annotated"
import { List, cons, nil } from "./BaseTypes"
import { DataValue, ExplValue } from "./DataValue"
import { DataType, ctrToDataType, elimToDataType } from "./DataType"
import { Env, emptyEnv } from "./Env"
import { Expl } from "./Expl"
import { Expr } from "./Expr"
import { Id, MemoId, Str, Value, _, fields, make, memoId } from "./Value"
import { at } from "./Versioned"

import Cont = Expr.Cont

// Unrelated to the annotation lattice. Expr case intentionally only defined for higher-order (function) case.
function join<K extends Cont> (κ: K, κʹ: K): K {
   const k: MemoId = memoId(join, arguments)
   if (κ instanceof Elim && κʹ instanceof Elim) {
      return DataElim.join<K>(κ, κʹ) as Cont as K
   } else
   if (κ instanceof Expr.Fun && κʹ instanceof Expr.Fun) {
      return Expr.fun(join(κ.σ, κʹ.σ))(k) as Expr as K
   } else {
      return absurd("Undefined join.", κ, κʹ)
   }
}

// Preorder traversal of all nodes in the matched prefix.
type MatchPrefix = List<ExplValue<DataValue>>

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
      return apply_(this, tv, nil())
   }
}

function apply_<K extends Cont> (σ: Elim<K>, tv: ExplValue, u̅: MatchPrefix): [Env, Match<K>] {
   if (VarElim.is(σ)) {
      return [Env.singleton(σ.x, tv), match(u̅, σ.κ)]
   } else
   if (DataElim.is(σ)) {
      const v: Value = tv.v,
            c: string = className(v)
      if (v instanceof DataValue) {
         const κ: K = (σ as any)[c] as K
         if (κ !== undefined) {
            const tv̅: ExplValue[] = Expl.explChildren(tv.t, v),
            [ρ, ξ]: [Env, Match<K>] = matchArgs(κ, tv̅, u̅)
            return [ρ, match(cons(tv as ExplValue<DataValue>, ξ.tv̅), ξ.κ)]
         } else {
            const d: DataType = elimToDataType.get(className(σ))!
            if (d.ctrs.has(c)) {
               return error(`Pattern mismatch: ${c} case is undefined for ${d.name.val} eliminator.`)
            } else {
               return error(`Pattern mismatch: found ${c}, expected ${d.name.val}.`)
            }
         }
      } else {
         return error(`Pattern mismatch: ${c} is not a datatype.`, v, σ)
      }
   } else {
      return absurd()
   }
}

// Parser ensures constructor calls are saturated.
function matchArgs<K extends Cont> (κ: K, tv̅: ExplValue[], u̅: MatchPrefix): [Env, Match<K>] {
   if (tv̅.length === 0) {
      return [emptyEnv(), match(u̅, κ)]
   } else {
      const [tv, ...tv̅ʹ] = tv̅
      if (κ instanceof Elim) {
         const σ: Elim<K> = κ, // "unfold" K into Elim<K>
               [ρ, ξ]: [Env, Match<K>] = apply_(σ, tv, u̅),
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

   static join<K extends Cont> (σ: Elim<K>, τ: Elim<K>): Elim<K> {
      const k: MemoId = memoId(DataElim.join, arguments)
      if (VarElim.is(σ) && VarElim.is(τ) && eq(σ.x, τ.x)) {
         return varElim(σ.x, join(σ.κ, τ.κ))(k)
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
         return dataElim(...cκ̅)(k)
      } else {
         return absurd("Undefined join.", σ, τ)
      }
   }
}

// cκ̅ non-empty and constructors all of the same datatype.
export function dataElim<K extends Cont> (...cκ̅: [string, K][]): (k: Id) => Elim<K> {
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
   return at(d.elimC as Class<DataElim<K>>, ...f̅)
}

export class VarElim<K extends Cont> extends Elim<K> {
   x: Str = _
   κ: K = _

   static is<K extends Cont> (σ: Elim<K>): σ is VarElim<K> {
      return σ instanceof VarElim
   }
}

export function varElim<K extends Cont> (x: Str, κ: K): (k: Id) => VarElim<K> {
   return at<VarElim<K>>(VarElim, x, κ)
}

export function apply_fwd (ξ: Match<Expr>): Annotation {
   return ξ.tv̅.toArray().reduce((α: Annotation, tv: ExplValue): Annotation => ann.meet(α, tv.t.__α), ann.top)
}

export function apply_bwd (ξ: Match<Expr>, α: Annotation): void {
   ξ.tv̅.toArray().map((tv: ExplValue): Value => setjoinα(α, tv.t))
}
