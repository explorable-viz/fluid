import { Eq } from "./Eq"

export interface Ord<K extends Ord<K>> extends Eq<K> {
   // The argument is always of the type implementing Ord.
   leq(a: K): boolean
}

export function eq<K extends Ord<K>>(a: K, b: K): boolean {
   return a.leq(b) && b.leq(a)
}

export type Comparator<T> = (x: T, y: T) => number

export interface JoinSemilattice<T> {
   join (...ts: T[]): T
   bot: T
}

export interface MeetSemilattice<T> {
   meet (...ts: T[]): T
   top: T
}

export interface Lattice<T> extends JoinSemilattice<T>, MeetSemilattice<T> {
}
