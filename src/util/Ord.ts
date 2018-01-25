export interface Ord<K extends Ord<K>> {
   // The argument is always of the type implementing Ord.
   leq(a: Ord<K>): boolean
}

export function eq<K extends Ord<K>>(a: K, b: K): boolean {
   return a.leq(b) && b.leq(a)
}

export type Comparator<T> = (x: T, y: T) => number
