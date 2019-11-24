import { __nonNull, absurd } from "./util/Core"
import { Annotation, bool_ } from "./util/Lattice"
import { __deltas } from "./Delta" 
import { Value, ValueTag, _ } from "./Value"

export function getα<T extends Value> (v: T): Annotation {
   return __annotations.get(v)
}

export function setα<T extends Value> (α: Annotation, v: T): T {
   if (getα(v) !== α) {
      __deltas.changed(v, { __α: { before: getα(v), after: α } })
   }
   __annotations.set(v, α)
   return v
}

export function negateallα<Tag extends ValueTag, T extends Value<Tag>> (v: T): void {
   __annotations.ann.forEach((α: Annotation, v: Value): void => {
      __annotations.ann.set(v, bool_.negate(α))
   })
}

export function setjoinα<T extends Value> (α: Annotation, v: T): T {
   return setα(bool_.join(α, getα(v)), v)
}

export function setmeetα<T extends Value> (α: Annotation, v: T): T {
   return setα(bool_.meet(α, getα(v)), v)
}

export enum Direction { Fwd, Bwd }

export class Annotations {
   ann: Map<Value, Annotation> = new Map()
   direction: Direction = Direction.Fwd

   // This is rather weak, in that we no longer require annotations to be set to top/bot upfront.
   get (v: Value): Annotation {
      const α: Annotation | undefined = this.ann.get(v)
      if (α !== undefined) {
         return α
      } else 
      if (this.direction === Direction.Fwd) {
         return bool_.top
      } else {
         return bool_.bot
      }
   }
   
   // Going forward, assume everything is available; annotation updates must be decreasing. 
   // Going backward, assume everything is not needed; annotation updates must be increasing.
   set (v: Value, α: Annotation): void {
      const current: Annotation | undefined = this.ann.get(v)
      if (current === undefined) {
         this.ann.set(v, α)
      } else
      if (this.direction === Direction.Fwd && α < current ||
          this.direction === Direction.Bwd && α > current) {
         this.ann.set(v, α)
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
