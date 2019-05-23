import { __nonNull } from "./Core"

export function flatten<T> (x̅̅: T[][]): T[] {
   return [].concat.apply([], x̅̅)
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
   return x̅.map((x: T, n: number): [T, U] => [x, y̅[n]])
}

export function unzip<T, U> (x̅: [T, U][]): [T[], U[]] {
   return [x̅.map(([x,]: [T, U]): T => x), x̅.map(([, y]: [T, U]): U => y)]
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
