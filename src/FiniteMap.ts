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

// Union with a combining function.
export function unionWith <V extends Persistent, T extends FiniteMap<V>> (m: T, mʹ: T, f: (v: V, vʹ: V) => V): T {
   if (NonEmpty.is(mʹ)) {
      const k: Str = mʹ.t.fst,
            v: V = mʹ.t.snd,
            vʹ: V | undefined = get(m, k),
            u: V = vʹ === undefined ? v : f(v, vʹ)
      return unionWith(insert(unionWith(m, mʹ.left, f), k, u), mʹ.right, f) as T
   } else
   if (Empty.is(mʹ)) {
      return m
   } else {
      return absurd()
   }
}
