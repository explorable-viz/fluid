import { __nonNull, absurd } from "./util/Core"
import { Annotation, bool_ } from "./util/Lattice"
import { __deltas } from "./Delta" 
import { Value, ValueTag, _ } from "./Value"

export function getα<T extends Value> (v: T): Annotation {
   return __annotations.get(v)
}

// Currently no deltas are associated with annotations.
export function setα<T extends Value> (α: Annotation, v: T): T {
   __annotations.set(v, α)
   return v
}

export function negateallα<Tag extends ValueTag, T extends Value<Tag>> (v: T): void {
   __annotations.direction = negate(__annotations.direction)
}

export function setjoinα<T extends Value> (α: Annotation, v: T): T {
   return setα(bool_.join(α, getα(v)), v)
}

export function setmeetα<T extends Value> (α: Annotation, v: T): T {
   return setα(bool_.meet(α, getα(v)), v)
}

export enum Direction { Fwd, Bwd }

function negate (direction: Direction): Direction {
   return direction === Direction.Fwd ? Direction.Bwd : Direction.Fwd
}

export class Annotations {
   ann: Set<Value> = new Set() // unavailable nodes (fwd) or needed nodes (bwd)
   direction: Direction = Direction.Fwd

   // Whether v is needed (going backward) or available (going forward).
   get (v: Value): Annotation {
      if (this.direction === Direction.Fwd) {
         return bool_.negate(this.ann.has(v))
      } else {
         return this.ann.has(v)
      }
   }
   
   // Going forward, annotation updates must be decreasing; going backward, increasing. This is because 
   // forward slicing propagates non-availability, whereas backward slicing propagates demand.
   set (v: Value, α: Annotation): void {
      const current: Annotation = this.get(v)
      if (this.direction === Direction.Fwd && α < current ||
          this.direction === Direction.Bwd && α > current) {
         this.ann.add(v)
      } else
      if (this.direction === Direction.Fwd && α > current ||
         this.direction === Direction.Bwd && α < current) {
         absurd(`Incompatible update of annotation from ${current} to ${α}.`, current, α)
      } else {
         // idempotent
      }
   }

   reset (direction: Direction): void {
      this.direction = direction
      this.ann.clear()
   }
}

export const __annotations = new Annotations()
