import { absurd, assert } from "./util/Core"
import { Eq } from "./util/Eq"
import { Persistent, State, Value, shallowEq } from "./Value"

export class Deltas {
   ẟ̅: Map<Value, Delta> = new Map()

   get size (): number {
      return this.ẟ̅.size
   }

   changed (v: Value, prop: string, u: Persistent): void {
      let v_ẟ: Delta | undefined = this.ẟ̅.get(v)
      if (v_ẟ === undefined) {
         this.ẟ̅.set(v, new Change({ [prop]: u }))
      } else
      if (v_ẟ instanceof Change) {
         const v_change: State = v_ẟ.changed
         if (v_change[prop] !== undefined) {
            assert(v_change[prop] === u)
         } else {
            v_change[prop] = u
         }
      } else {
         absurd()
      }
   }

   reclassified (v: Value): void {
      let v_ẟ: Delta | undefined = this.ẟ̅.get(v)
      if (v_ẟ === undefined) {
         this.ẟ̅.set(v, new Reclassify())
      } else
      if (v_ẟ instanceof Reclassify) {
         // ok
      } else {
         absurd()
      }
   }

   created (v: Value): void {
      let v_ẟ: Delta | undefined = this.ẟ̅.get(v)
      if (v_ẟ === undefined) {
         this.ẟ̅.set(v, new New())
      } else
      if (v_ẟ instanceof New) {
         // ok
      } else {
         absurd()
      }
   }

   clear (): void {
      this.ẟ̅.clear()
   }
}

export const __deltas: Deltas = new Deltas()

export abstract class Delta implements Eq<Delta> {
   abstract eq (ẟ: Delta): boolean
}

export class New extends Delta {
   eq (ẟ: Delta): boolean {
      return ẟ instanceof New
   }
}

export class Change extends Delta {
   changed: State

   constructor (changed: State) {
      super()
      this.changed = changed
   }

   eq (ẟ: Delta): boolean {
      return ẟ instanceof Change && shallowEq(this.changed, ẟ.changed)
   }
}

// Constructor has changed, and therefore fields may not align. A more sophisticated reclassification
// delta could allow for fields to be shared when an object changes class.
export class Reclassify extends Delta {
   eq (ẟ: Delta): boolean {
      return ẟ instanceof Reclassify
   }
}
