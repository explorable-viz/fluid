import { assert} from "./util/Core"
import { __def, key } from "./Memo"
import { create } from "./Runtime"

// Hash-consed finite maps. Sensitive to key changes, which cause the identity of subtrees to change.
export abstract class FiniteMap<V> {
   abstract get (k: string): V | undefined // ES6 map-style signature

   has (k:string): boolean {
      return this.get(k) !== undefined
   }
}

export class Empty<V> extends FiniteMap<V> {
   static is<V> (m: FiniteMap<V>): m is Empty<V> {
      return m instanceof Empty
   }

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

   static is<V> (m: FiniteMap<V>): m is NonEmpty<V> {
      return m instanceof NonEmpty
   }

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
   if (NonEmpty.is(m)) { // TS bug requires this branch first
      if (k <= m.k) {
         if (m.k <= k) {
            return NonEmpty.at(α, m.left, k, v, m.right)
         } else {
            return NonEmpty.at(α, insert(m.left, k, v), m.k, m.v, m.right)
         }
      } else {
         return NonEmpty.at(α, m.left, m.k, m.v, insert(m.right, k, v))
      }
   } else
   if (Empty.is(m)) {
      return NonEmpty.at(α, m, k, v, m)
   } else {
      return assert(false)
   }
}

export function extend<V> (m: FiniteMap<V>, kvs: [string, V][]): FiniteMap<V> {
   kvs.forEach(([k, v]): void => {
      m = insert(m, k, v)
   })
   return m
}

// Left-biased union, meaning m is preferred to mʹ when duplicate keys are encountered.
// TODO: why call this "union" then?
export function union<V> (m: FiniteMap<V>, mʹ: FiniteMap<V>): FiniteMap<V> {
   return assert(false, "TODO")
}
