import { __def, key, keyP } from "./Memo"
import { create } from "./Runtime"

// Hash-consed finite maps. Sensitive to key changes, which cause the identity of subtrees to change.
type FiniteMap<V> = Empty | NonEmpty<V>

export class Empty {
   static at (α: Addr): Empty {
      const this_: Empty = create(α, Empty)
      this_.__version()
      return this_
   }
}

export class NonEmpty<V> {
   left: FiniteMap<V>
   k: string
   v: V
   right: FiniteMap<V>

   static at<V> (α: Addr, left: FiniteMap<V>, k: string, v: V, right: FiniteMap<V>): NonEmpty<V> {
      const this_: NonEmpty<V> = create<NonEmpty<V>>(α, NonEmpty)
      this_.left = left
      this_.k = k
      this_.v = v
      this_.right = right
      this_.__version()
      return this_
   }
}

__def(empty)
export function empty (): Empty {
   return Empty.at(key(empty, arguments))
}

__def(insert)
export function insert<V> (m: FiniteMap<V>, k: string, v: V): FiniteMap<V> {
   const α: Addr = key(insert, arguments)
   if (m instanceof Empty) {
      return NonEmpty.at(α, m, k, v, m)
   } else {
      if (k <= m.k) {
         if (m.k <= k) {
            return NonEmpty.at(α, m.left, k, v, m.right)
         } else {
            return NonEmpty.at(α, insert(m.left, k, v), m.k, m.v, m.right)
         }
      } else {
         return NonEmpty.at(α, m.left, m.k, m.v, insert(m.right, k, v))
      }
   }
}

export function extend<V> (m: FiniteMap<V>, kvs: [string, V][]): FiniteMap<V> {
   kvs.forEach(([k, v]): void => {
      m = insert(m, k, v)
   })
   return m
}

/*
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