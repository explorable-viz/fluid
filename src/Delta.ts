import { absurd, assert } from "./util/Core"
import { Eq } from "./util/Eq"
import { State, Value, eq, leq } from "./Value"

export class Deltas {
   ẟ̅: Map<Value, Delta> = new Map()

   get size (): number {
      return this.ẟ̅.size
   }

   // Updates to a change set must be increasing (at a given revision). Because of sharing within
   // a revision, a node may first appear "new" and then appear "changed"; same condition applies.
   changed (v: Value, s: State): void {
      let v_ẟ: Delta | undefined = this.ẟ̅.get(v)
      if (v_ẟ === undefined) {
         this.ẟ̅.set(v, new Change(s))
      } else
      if (v_ẟ instanceof Change) {
         assert(leq(v_ẟ.changed, s))
         v_ẟ.changed = s
      } else
      if (v_ẟ instanceof New) {
         assert(leq(v_ẟ.state, s))
         v_ẟ.state = s
      } else {
         absurd()
      }
   }

   reclassified (v: Value, s: State): void {
      let v_ẟ: Delta | undefined = this.ẟ̅.get(v)
      if (v_ẟ === undefined) {
         this.ẟ̅.set(v, new Reclassify(s))
      } else
      if (v_ẟ instanceof Reclassify) {
         absurd()
      } else {
         absurd()
      }
   }

   created (v: Value, s: State): void {
      let v_ẟ: Delta | undefined = this.ẟ̅.get(v)
      if (v_ẟ === undefined) {
         this.ẟ̅.set(v, new New(s))
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
   state: State

   constructor (state: State) {
      super()
      this.state = state
   }

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

   leq (ẟ: Delta): boolean {
      return ẟ instanceof Change && leq(this.changed, ẟ.changed)
   }

   eq (ẟ: Delta): boolean {
      return ẟ instanceof Change && eq(this.changed, ẟ.changed)
   }
}

// Constructor has changed, and therefore fields may not align. More sophisticated reclassification
// delta could allow for fields to be shared when an object changes class.
export class Reclassify extends Delta {
   state: State

   constructor (state: State) {
      super()
      this.state = state
   }

   eq (ẟ: Delta): boolean {
      return ẟ instanceof Reclassify
   }
}
