export interface Ord<K> {
   leq (a: K): boolean
}

export interface String extends Ord<String> {
}

// TODO: fix interface above so I don't need this cast.
(<any>String.prototype).leq = function (a: String): boolean {
   return this <= a
}

export function eq <K extends Ord<K>> (a: K, b: K): boolean {
   return a.leq(b) && b.leq(a)
}
