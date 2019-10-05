import { absurd } from "./util/Core"
import { Empty, NonEmpty, Pair, Tree, empty, nonEmpty, pair } from "./BaseTypes"
import { Persistent, Str } from "./Value"

// Simplify to keys of type Str until we have something like type classes.
export type FiniteMap<V> = Tree<Pair<Str, V>>

export function get <V extends Persistent> (m: FiniteMap<V>, k: Str): V | undefined {
   if (NonEmpty.is(m)) {
      if (k.leq(m.t.fst)) {
         if (m.t.fst.leq(k)) {
            return m.t.snd
         } else {
            return get(m.left, k)
         }
      } else {
         return get(m.right, k)
      }
   } else
   if (Empty.is(m)) {
      return undefined
   } else {
      return absurd()
   }
}

export function insert <V extends Persistent> (m: FiniteMap<V>, k: Str, v: V): FiniteMap<V> {
   if (NonEmpty.is(m)) {
      if (k.leq(m.t.fst)) {
         if (m.t.fst.leq(k)) {
            return nonEmpty(m.left, pair(k, v), m.right)
         } else {
            return nonEmpty(insert(m.left, k, v), m.t, m.right)
         }
      } else {
         return nonEmpty(m.left, m.t, insert(m.right, k, v))
      }
   } else
   if (Empty.is(m)) {
      return nonEmpty(m, pair(k, v), m)
   } else {
      return absurd()
   }
}

export function singleton <V extends Persistent> (k: Str, v: V): FiniteMap<V> {
   return insert(empty(), k, v)
}

// Union with a combining function. If keys are equal, right-hand key will be used in the output.
// Avoid primes in signature; seems to be incompatible with version of ts-loader used by Wrattler.
export function unionWith <V extends Persistent, T extends FiniteMap<V>> (m1: T, m2: T, f: (v1: V, v2: V) => V): T {
   if (NonEmpty.is(m2)) {
      const k: Str = m2.t.fst,
            v: V = m2.t.snd,
            vʹ: V | undefined = get(m1, k),
            u: V = vʹ === undefined ? v : f(v, vʹ)
      return unionWith(insert(unionWith(m1, m2.left, f), k, u), m2.right, f) as T
   } else
   if (Empty.is(m2)) {
      return m1
   } else {
      return absurd()
   }
}
