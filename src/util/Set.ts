export function diff<T> (xs: Set<T>, ys: Set<T>): Set<T> {
   return filter(xs, x => !(ys.has(x)))
}

export function every<T>(xs: Set<T>, pred: (x: T) => boolean): boolean {
   return Array.from(xs).every(pred)
}

export function filter<T> (xs: Set<T>, pred: (x: T) => boolean): Set<T> {
   const ys: Set<T> = new Set
   xs.forEach(x => {
      if (pred(x)) {
         ys.add(x)
      }
   })
   return ys
}

export function map<T, U> (xs: Set<T>, f: (x: T) => U): Set<U> {
   const ys: Set<U> = new Set
   xs.forEach(x => ys.add(f(x)))
   return ys
}

export function some<T>(xs: Set<T>, pred: (x: T) => boolean): boolean {
   return Array.from(xs).some(pred)
}

export function union<T>(...xss: Set<T>[]): Set<T> {
   const ys: Set<T> = new Set
   xss.forEach(xs => {
      xs.forEach(x => {
         ys.add(x)
      })
   })
   return ys
}
