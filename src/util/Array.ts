import { __nonNull } from "./Core"

export function flatten<T>(xss: T[][]): T[] {
   return [].concat.apply([], xss)
}

export function counts<T>(xs: T[]): Map<T, number> {
   const counts: Map<T, number> = new Map
   xs.forEach(x => {
      if (counts.has(x)) {
         counts.set(x, __nonNull(counts.get(x)) + 1)
      } else {
         counts.set(x, 1)
      }
   })
   return counts
}

export function zip<T, U> (xs: T[], ys: U[]): [T, U][] {
   return xs.map((x: T, n: number): [T, U] => [x, ys[n]])
}
