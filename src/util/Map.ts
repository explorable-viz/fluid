import { __nonNull } from "./Core"

// Array.from doesn't seem to work in Karma singleRun mode.
export function toArray<K, V>(m: Map<K, V>): [K, V][] {
   const kvs: [K, V][] = []
   m.forEach((v, k) => {
      kvs.push([k, v])
   })
   return kvs
}

export function filter<K, V>(m: Map<K, V>, pred: (v: V) => boolean): Map<K, V> {
   return new Map(toArray(m).filter(([_, v]) => pred(v)))
}

export function map<K, V, U>(m: Map<K, V>, f: (u: V) => U): Map<K, U> {
   return new Map(toArray(m).map(([k, v]): [K, U] => [k, f(v)]))
}

export function extend <K, V> (m: Map<K, V>, kvs: [K, V][]): Map<K, V> {
   const m_: Map<K, V> = new Map(m)
   kvs.forEach(([k, v]) => {
      m_.set(k, v)
   })
   return m_
}

export function intersectionWith<K, V, U>(
   ms: Map<K, V>[],
   f: (vs: V[]) => U
): Map<K, U> {
   if (ms.length === 0) {
      return new Map()
   } else {
      const m_: Map<K, U> = new Map()
      const ms_: Map<K, V>[] = ms.slice(1)
      ms[0].forEach((v, k) => {
         let vs: V[] = [v]
         ms_.forEach(m => {
            if (m.has(k)) {
               vs.push(__nonNull(m.get(k)))
            }
         })
         if (vs.length === ms.length) {
            m_.set(k, f(vs))
         }
      })
      return m_
   }
}

export function keys<K, V>(m: Map<K,V>): Set<K> {
   const ks: Set<K> = new Set
   m.forEach((_: V, k: K): void => {
      ks.add(k)
   })
   return ks
}

export function unionWith<K, V, U>(
   ms: Map<K, V>[],
   f: (vs: V[]) => U
): Map<K, U> {
   if (ms.length === 0) {
      return new Map()
   } else {
      const m_: Map<K, V[]> = new Map()
      ms.forEach(m => {
         m.forEach((v, k) => {
            if (m_.has(k)) {
               __nonNull(m_.get(k)).push(v)
            } else {
               m_.set(k, [v])
            }
         })
      })
      const m: Map<K, U> = new Map()
      m_.forEach((vs, k) => {
         m.set(k, f(vs))
      })
      return m
   }
}

// Left-biased union.
export function union<K, V>(ms: Map<K, V>[]): Map<K, V> { 
   return unionWith(ms, (vs: V[]): V => vs[0])
}

export function values<K, V>(m: Map<K,V>): Set<V> {
   const vs: Set<V> = new Set
   m.forEach((v: V): void => {
      vs.add(v)
   })
   return vs
}
