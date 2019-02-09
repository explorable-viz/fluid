import { Eq } from "./Eq"

export interface Ord<K extends Ord<K>> extends Eq<K> {
   // The argument is always of the type implementing Ord.
   leq(a: K): boolean
}

export function eq<K extends Ord<K>>(a: K, b: K): boolean {
   return a.leq(b) && b.leq(a)
}

// Seem to need to put Eq and Leq implementation in the same "global" declaration.
declare global {
   interface String {
      leq (str: string): boolean
      eq (str: string): boolean
   }

   interface Number {
      leq (n: number): boolean
      eq (n: number): boolean
   }
} 

String.prototype.leq = function (str: string): boolean {
   return this <= str
}

String.prototype.eq = function (str: string): boolean {
   return this === str
}

Number.prototype.leq = function (n: number): boolean {
   return this <= n
}

Number.prototype.eq = function (n: number): boolean {
   return this === n
}

export type Comparator<T> = (x: T, y: T) => number

export interface JoinSemilattice<T> {
   join (t: T): T
}
