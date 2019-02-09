import { __nonNull, assert } from "./Core"

// Enforces validity of index, permitting an undefined index (to allow for
// zero-length arrays).
export class ArrayCursor<T> {
   _n: number | undefined = undefined
   readonly array: T[]

   get n (): number | undefined {
      return this._n
   }

   set n (n: number | undefined) {
      assert(n === undefined || n < this.array.length, "Array index out of bounds")
      this._n = n
   }

   get current () {
      assert(this.n !== undefined && this.n < this.array.length, "Array index out of bounds")
      return this.array[__nonNull(this.n)]
   }

   constructor(array: T[]) {
      this.array = Array.from(array)
   }

   next (): boolean {
      if (this.n !== undefined) {
         if (this.n < this.array.length - 1) {
            ++this.n
            return true
         } else {
            return false
         }
      } else {
         return assert(false)
      }
   }

   previous (): boolean {
      if (this.n !== undefined) {
         if (this.n > 0) {
            --this.n
            return true
         } else {
            return false
         }
      } else {
         return assert(false)
      }
   }
}
