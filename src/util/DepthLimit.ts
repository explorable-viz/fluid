// Some hackery to detect runaway recursion.
class DepthLimit {
   max: number
   depth: number

   constructor(max: number) {
      this.max = max
      this.depth = 0
   }

   maxExceeded(): boolean {
      return this.depth > this.max
   }

   increment(): DepthLimit {
      ++this.depth
      return this
   }

   decrement(): DepthLimit {
      --this.depth
      return this
   }

   info(): string {
      return "Truncating recursion at depth " + this.depth
   }
}

export const __depth = new DepthLimit(200)
