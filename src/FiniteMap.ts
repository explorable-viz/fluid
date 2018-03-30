import { assert} from "./util/Core"
import { __def, key } from "./Memo"
import { create } from "./Runtime"

// Hash-consed finite maps, implemented as snoc lists, so the identity of an environment can correspond 
// to a sequence of substitutions. A tree implementation would make the identity of an environment sensitive
// to keys, which would in turn require addressable strings to allow memo hits across changes to names.
// Moreover environment identity depending on sort order of names would incrementalise quite differently.
export abstract class FiniteMap<V> {
   abstract get (k: string): V | undefined // ES6 map-style signature

   has (k:string): boolean {
      return this.get(k) !== undefined
   }

   __FiniteMap(): void {
      // discriminator (type-compatible with ES6 maps for as long as I abuse Object)
   }
}

export class Nil<V> extends FiniteMap<V> {
   static is<V> (m: FiniteMap<V>): m is Nil<V> {
      return m instanceof Nil
   }

   static at<V> (α: Addr): Nil<V> {
      const this_: Nil<V> = create<Nil<V>>(α, Nil)
      this_.__version()
      return this_
   }

   get (k: string): V | undefined {
      return undefined 
   }
}

export class Snoc<V> extends FiniteMap<V> {
   tail: FiniteMap<V>
   k: string
   v: V

   static is<V> (m: FiniteMap<V>): m is Snoc<V> {
      return m instanceof Snoc
   }

   static at<V> (α: Addr, tail: FiniteMap<V>, k: string, v: V): Snoc<V> {
      const this_: Snoc<V> = create<Snoc<V>>(α, Snoc)
      this_.tail = tail
      this_.k = k
      this_.v = v
      this_.__version()
      return this_
   }

   get (k: string): V | undefined {
      if (this.k === k) {
         return this.v
      } else {
         return this.tail.get(k)
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

// Right-biased union (overrides m with the bindings in mʹ).
// Called concat by analogy with notation used in the paper.
export function concat<V> (m: FiniteMap<V>, mʹ: FiniteMap<V>): FiniteMap<V> {
   if (NonEmpty.is(mʹ)) {
      return insert(concat(concat(m, mʹ.left), mʹ.right), mʹ.k, mʹ.v)
   } else
   if (Empty.is(mʹ)) {
      return m
   } else {
      return assert(false)
   }
}
