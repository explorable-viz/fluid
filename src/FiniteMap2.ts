import { absurd } from "./util/Core"
import { Ord } from "./util/Ord"
import { Empty, NonEmpty, Pair, Tree, empty, nonEmpty, pair } from "./BaseTypes2"
import { Persistent } from "./Value2"

export type FiniteMap<K extends Ord<K>, V> = Tree<Pair<K, V>>

export function get <K extends Ord<K> & Persistent, V extends Persistent> (m: FiniteMap<K, V>, k: K): V | undefined {
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

export function insert <K extends Ord<K> & Persistent, V extends Persistent> (m: FiniteMap<K, V>, k: K, v: V): FiniteMap<K, V> {
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

export function singleton <K extends Ord<K> & Persistent, V extends Persistent> (k: K, v: V): FiniteMap<K, V> {
   return insert(empty(), k, v)
}

// Union with a combining function.
export function unionWith <K extends Ord<K> & Persistent, V extends Persistent> (
   m: FiniteMap<K, V>,
   mʹ: FiniteMap<K, V>,
   f: (v: V, vʹ: V) => V
): FiniteMap<K, V> {
   if (NonEmpty.is(mʹ)) {
      const k: K = mʹ.t.fst,
            v: V = mʹ.t.snd,
            vʹ: V | undefined = get(m, k),
            u: V = vʹ === undefined ? v : f(v, vʹ)
      return unionWith(insert(unionWith(m, mʹ.left, f), k, u), mʹ.right, f)
   } else
   if (Empty.is(mʹ)) {
      return m
   } else {
      return absurd()
   }
}
