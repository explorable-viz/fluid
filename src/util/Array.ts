import { __nonNull, assert } from "./Core"

export function flatten<T> (x̅̅: T[][]): T[] {
   const x̅: T[] = [] // otherwise TS is confused
   return x̅.concat.apply([], x̅̅)
}

export function counts<T> (x̅: T[]): Map<T, number> {
   const counts: Map<T, number> = new Map
   x̅.forEach(x => {
      if (counts.has(x)) {
         counts.set(x, __nonNull(counts.get(x)) + 1)
      } else {
         counts.set(x, 1)
      }
   })
   return counts
}

export function zip<T, U> (x̅: T[], y̅: U[]): [T, U][] {
   return zipWith((t: T, u: U): [T, U] => [t, u])(x̅, y̅)
}

export function zipWith<T, U, V> (f: (t: T, u: U) => V): (x̅: T[], y̅: U[]) => V[] {
   return (x̅, y̅) => x̅.map((x: T, n: number): V => f(x, y̅[n]))
}

export function includes<T> (x̅: T[], y̅: T[]): boolean {
   return y̅.every(y => x̅.includes(y))
}

export function eq<T> (x̅: T[], y̅: T[]): boolean {
   let n: number = x̅.length
   if (n != y̅.length) {
      return false
   } else {
      while (n--) {
         if (x̅[n] !== y̅[n]) return false;
      }
      return true
   }
}

export function nth<T> (x̅: T[], n: number): T {
   assert(n < x̅.length)
   return x̅[n]
}
