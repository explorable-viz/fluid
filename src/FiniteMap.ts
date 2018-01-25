import { Cons, Empty, List, Nil, None, NonEmpty, Pair, Prim, Some, Tree, __forEach } from "./BaseTypes"
import { Ord } from "./Ord"
import { __def, key, keyP } from "./Memo"
import { assertMessage } from "./Util"

// Hash-consed finite maps. Sensitive to key changes, which cause the identity of subtrees to change.
export type FiniteMap<K extends Ord<K>, V> = Tree<Pair<K, V>>

__def(empty)
export function empty <K extends Ord<K>, V> (): FiniteMap<K, V> {
   return Empty.at(key(empty, arguments))
}

__def(insert)
export function insert <K extends Ord<K>, V> (m: FiniteMap<K, V>, k: K, v: V): FiniteMap<K, V> {
   const α: Addr = key(insert, arguments)
   return m.__visit({
      is_Empty: (_) =>
         NonEmpty.at_(α, m, Pair.at_(keyP(α, 't', 'val'), k, v), m),
      is_NonEmpty: (m) => {
         if (k.leq(m.t.fst)) {
            if (m.t.fst.leq(k)) {
               return NonEmpty.at_(α, m.left, Pair.at_(keyP(α, 't', 'val'), k, v), m.right)
            } else {
               return NonEmpty.at_(α, insert(m.left, k, v), m.t, m.right)
            }
         } else {
            return NonEmpty.at_(α, m.left, m.t, insert(m.right, k, v))
         }
      }
   })
}

__def(singleton)
export function singleton <K extends Ord<K>, V> (k: K, v: V): FiniteMap<K, V> {
   return insert(empty(), k, v)
}

__def(extend)
export function extend <K extends Ord<K>, V> (m: FiniteMap<K, V>, kvs: List<Pair<K, V>>): FiniteMap<K, V> {
   __forEach(kvs, (kv): void => {
      m = insert(m, kv.fst, kv.snd)
   })
   return m
}

// Union with a combining function.
__def(unionWith)
export function unionWith <K extends Ord<K>, V> (
   m: FiniteMap<K, V>,
   mʹ: FiniteMap<K, V>,
   f: (v: V, vʹ: V) => V
): FiniteMap<K, V> {
   return mʹ.__visit({
      is_Empty: (_) =>
         m,
      is_NonEmpty: (mʹ) => {
         const k: K = mʹ.t.fst,
               v: V = mʹ.t.snd
         const u: V = get(m, k).__visit({
            is_None: (_) => v,
            is_Some: (vʹ) => f(v, vʹ.valOf)
         })
         return unionWith(insert(unionWith(m, mʹ.left, f), k, u), mʹ.right, f)
      }
   })
}

// Left-biased union, meaning m is preferred to mʹ when duplicate keys are encountered.
__def(union)
export function union <K extends Ord<K>, V> (m: FiniteMap<K, V>, mʹ: FiniteMap<K, V>): FiniteMap<K, V> {
   return assertMessage(false, 'TODO')
}

__def(get)
export function get <K extends Ord<K>, V> (m: FiniteMap<K, V>, k: K): Prim.Option<V> {
   const α: Addr = key(get, arguments)
   return m.__visit({
      is_Empty: (_) =>
         None.at(α),
      is_NonEmpty: (m) => {
         if (k.leq(m.t.fst)) {
            if (m.t.fst.leq(k)) {
               return Some.at_(α, m.t.snd)
            } else {
               return get(m.left, k)
            }
         } else {
            return get(m.right, k)
         }
      }
   })
}

__def(keys)
export function keys <K extends Ord<K>, V> (m: FiniteMap<K, V>): List<K> {
   return keys_acc<K, V>(m, Nil.at<K>(key(keys, arguments)))
}

__def(keys_acc)
function keys_acc <K extends Ord<K>, V> (m: FiniteMap<K, V>, acc: List<K>): List<K> {
   return m.__visit({
      is_Empty: (_) =>
         acc,
      is_NonEmpty: function (m) {
         return keys_acc(m.left, Cons.at_(key(keys_acc, arguments), m.t.fst, keys_acc(m.right, acc)))
      }
   })
}
