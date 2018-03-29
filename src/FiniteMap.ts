import { __def, key, keyP } from "./Memo"
import { create } from "./Runtime"

// Hash-consed finite maps. Sensitive to key changes, which cause the identity of subtrees to change.
abstract class FiniteMap<V> {
   // ES6 map-style interface
   abstract get (k: string): V | undefined
}

export class Empty<V> extends FiniteMap<V> {
   static at<V> (α: Addr): Empty<V> {
      const this_: Empty<V> = create<Empty<V>>(α, Empty)
      this_.__version()
      return this_
   }

   get (k: string): V | undefined {
      return undefined 
   }
}

export class NonEmpty<V> extends FiniteMap<V> {
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

   get (k: string): V | undefined {
      if (k <= this.k) {
         if (this.k <= k) {
            return this.v
         } else {
            return this.left.get(k)
         }
      } else {
         return this.right.get(k)
      }
   }
}

__def(empty)
export function empty<V> (): Empty<V> {
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
