import { assert } from "./util/Core"
import { Ord } from "./util/Ord"
import { Empty, NonEmpty, Pair, Tree } from "./BaseTypes"
import { Persistent } from "./Runtime"

// Interned finite maps. Sensitive to key changes, which cause the identity of subtrees to change.
export type FiniteMap<K extends Ord<K> & Persistent, V extends Persistent> = Tree<Pair<K, V>>

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
      return assert(false)
   }
}

export function has <K extends Ord<K> & Persistent, V extends Persistent> (m: FiniteMap<K, V>, k: K): boolean {
   return get(m, k) !== undefined
}

export function insert <K extends Ord<K> & Persistent, V extends Persistent> (m: FiniteMap<K, V>, k: K, v: V): FiniteMap<K, V> {
   if (NonEmpty.is(m)) {
      if (k.leq(m.t.fst)) {
         if (m.t.fst.leq(k)) {
            return NonEmpty.make(m.left, Pair.make(k, v), m.right)
         } else {
            return NonEmpty.make(insert(m.left, k, v), m.t, m.right)
         }
      } else {
         return NonEmpty.make(m.left, m.t, insert(m.right, k, v))
      }
   } else
   if (Empty.is(m)) {
      return NonEmpty.make(m, Pair.make(k, v), m)
   } else {
      return assert(false)
   }
}

export function singleton <K extends Ord<K> & Persistent, V extends Persistent> (k: K, v: V): FiniteMap<K, V> {
   return insert(Empty.make(), k, v)
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
      return assert(false)
   }
}
