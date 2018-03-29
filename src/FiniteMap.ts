import { Ord } from "./util/Ord"
import { __def, key, keyP } from "./Memo"
import { create } from "./Runtime"

// Hash-consed finite maps. Sensitive to key changes, which cause the identity of subtrees to change.
type FiniteMap<K, V> = Empty<K, V> | NonEmpty<K, V>

export class Empty<K, V> {
   static at<K, V> (α: Addr): Empty<K, V> {
      const this_: Empty<K, V> = create(α, Empty)
      this_.__version()
      return this_
   }
}

export class NonEmpty<K, V> {
   left: FiniteMap<K, V>
   k: K
   v: V
   right: FiniteMap<K, V>

   static at<K, V> (α: Addr, left: FiniteMap<K, V>, k: K, v: V, right: FiniteMap<K, V>): NonEmpty<K, V> {
      const this_: NonEmpty<K, V> = create<NonEmpty<K, V>>(α, NonEmpty)
      this_.left = left
      this_.k = k
      this_.v = v
      this_.right = right
      this_.__version()
      return this_
   }
}
/*

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

__def(extend)
export function extend <K extends Ord<K>, V> (m: FiniteMap<K, V>, kvs: List<Pair<K, V>>): FiniteMap<K, V> {
   __forEach(kvs, (kv): void => {
      m = insert(m, kv.fst, kv.snd)
   })
   return m
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
*/