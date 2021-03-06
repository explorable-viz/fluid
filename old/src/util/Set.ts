export function diff<T> (x̅: Set<T>, y̅: Set<T>): Set<T> {
   return filter(x̅, x => !(y̅.has(x)))
}

export function every<T> (x̅: Set<T>, pred: (x: T) => boolean): boolean {
   return Array.from(x̅).every(pred)
}

export function filter<T> (x̅: Set<T>, pred: (x: T) => boolean): Set<T> {
   return new Set([...x̅].filter(pred))
}

export function map<T, U> (x̅: Set<T>, f: (x: T) => U): Set<U> {
   return new Set([...x̅].map(f))
}

export function some<T> (x̅: Set<T>, pred: (x: T) => boolean): boolean {
   return Array.from(x̅).some(pred)
}

export function union<T> (...x̅̅: Set<T>[]): Set<T> {
   const y̅: Set<T> = new Set()
   x̅̅.forEach(x̅ => {
      x̅.forEach(x => {
         y̅.add(x)
      })
   })
   return y̅
}

export function intersection<T> (x̅̅: Set<T>, y̅: Set<T>): Set<T> {
   const zs: Set<T> = new Set()
   x̅̅.forEach(x => {
      if (y̅.has(x)) {
         zs.add(x)
      }
   })
   return zs
}
