import { __nonNull } from "./Core"

// Array.from doesn't seem to work in Karma singleRun mode.
export function toArray<K, V> (m: Map<K, V>): [K, V][] {
   const kvs: [K, V][] = []
   m.forEach((v, k) => {
      kvs.push([k, v])
   })
   return kvs
}

export function filter<K, V> (m: Map<K, V>, pred: (v: V) => boolean): Map<K, V> {
   return new Map(toArray(m).filter(([_, v]) => pred(v)))
}

export function map<K, V, U> (m: Map<K, V>, f: (u: V) => U): Map<K, U> {
   return new Map(toArray(m).map(([k, v]): [K, U] => [k, f(v)]))
}

export function extend <K, V> (m: Map<K, V>, kvs: [K, V][]): Map<K, V> {
   const mʹ: Map<K, V> = new Map(m)
   kvs.forEach(([k, v]) => {
      mʹ.set(k, v)
   })
   return mʹ
}

export function intersectionWith<K, V, U> (m̅: Map<K, V>[], f: (v̅: V[]) => U): Map<K, U> {
   if (m̅.length === 0) {
      return new Map()
   } else {
      const mʹ: Map<K, U> = new Map()
      const m̅ʹ: Map<K, V>[] = m̅.slice(1)
      m̅[0].forEach((v, k) => {
         const vs: V[] = [v]
         m̅ʹ.forEach(m => {
            if (m.has(k)) {
               vs.push(__nonNull(m.get(k)))
            }
         })
         if (vs.length === m̅.length) {
            mʹ.set(k, f(vs))
         }
      })
      return mʹ
   }
}

export function keys<K, V> (m: Map<K,V>): Set<K> {
   const k̅: Set<K> = new Set
   m.forEach((_: V, k: K): void => {
      k̅.add(k)
   })
   return k̅
}

export function unionWith<K, V, U> (m̅: Map<K, V>[], f: (vs: V[]) => U): Map<K, U> {
   if (m̅.length === 0) {
      return new Map()
   } else {
      const mʹ: Map<K, V[]> = new Map()
      m̅.forEach(m => {
         m.forEach((v, k) => {
            if (mʹ.has(k)) {
               __nonNull(mʹ.get(k)).push(v)
            } else {
               mʹ.set(k, [v])
            }
         })
      })
      const m: Map<K, U> = new Map()
      mʹ.forEach((vs, k) => {
         m.set(k, f(vs))
      })
      return m
   }
}

// Left-biased union.
export function union<K, V> (m̅: Map<K, V>[]): Map<K, V> { 
   return unionWith(m̅, (v̅: V[]): V => v̅[0])
}

export function values<K, V> (m: Map<K,V>): Set<V> {
   const v̅: Set<V> = new Set
   m.forEach((v: V): void => {
      v̅.add(v)
   })
   return v̅
}
