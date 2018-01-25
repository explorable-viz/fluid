import { Ord } from "./util/Ord"
import { create } from "./Runtime"

export class Int {
   val: number

   static at (α: Addr, val: number): Int {
      const this1: Int = create(α, Int)
      this1.val = val
      this1.__version()
      return this1
   }

   toString (): string {
      return this.val.toString()
   }
}

export class Str implements Ord<Str> {
   val: string

   static at (α: Addr, val: string): Str {
      const this1: Str = create(α, Str)
      this1.val = val
      this1.__version()
      return this1
   }

   toString (): string {
      return this.val
   }

   leq (that: Str): boolean {
      return this.val <= that.val
   }
}
