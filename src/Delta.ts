import { absurd, assert } from "./util/Core"
import { Ord } from "./util/Ord"
import { Persistent, Value, fields, mergeInto } from "./Value"

// Difference between two states (of the same value class). Should probably make this more like Value, and
// parameterised on type of values being compared.
export interface ValueDelta {
   [prop: string]: { before: Persistent, after: Persistent } // [before, after]
}

export function leq (s1: ValueDelta, s2: ValueDelta): boolean {
   return Object.keys(s1).every((prop: string): boolean => {
      return s2.hasOwnProperty(prop) && s1[prop].before === s2[prop].before && s1[prop].after === s2[prop].after
   })
}

function empty (ẟ: ValueDelta): boolean {
   return fields(ẟ).length === 0
}

export class Deltas {
   ẟ̅: Map<Value, Delta> = new Map()

   get size (): number {
      return this.ẟ̅.size
   }

   // Change sets must be disjoint at a given revision. Because of sharing within a revision, 
   // a node may first appear new (or reclassified) and then later appear changed, but the 
   // subsequent change sets must be empty.
   changed (v: Value, s_ẟ: ValueDelta): void {
      let v_ẟ: Delta | undefined = this.ẟ̅.get(v)
      if (v_ẟ === undefined) {
         this.ẟ̅.set(v, new Change(s_ẟ))
      } else
      if (v_ẟ instanceof Change) {
         mergeInto(v_ẟ.changed, s_ẟ)
      } else
      if (v_ẟ instanceof New || v_ẟ instanceof Reclassify) {
         assert(empty(s_ẟ))
      } else {
         absurd()
      }
   }

   // A value cannot be reclassified twice at the same revision.
   reclassified (v: Value): void {
      let v_ẟ: Delta | undefined = this.ẟ̅.get(v)
      if (v_ẟ === undefined) {
         this.ẟ̅.set(v, new Reclassify())
      } else {
         absurd()
      }
   }

   // A value cannot be created twice at the same revision.
   created (v: Value): void {
      let v_ẟ: Delta | undefined = this.ẟ̅.get(v)
      if (v_ẟ === undefined) {
         this.ẟ̅.set(v, new New())
      } else {
         absurd()
      }
   }

   clear (): void {
      this.ẟ̅.clear()
   }
}

export const __deltas: Deltas = new Deltas()

export abstract class Delta implements Ord<Delta> {
   abstract leq (ẟ: Delta): boolean

   eq (ẟ: Delta): boolean {
      return this.leq(ẟ) && ẟ.leq(this)
   }
}

export class New extends Delta {
   constructor () {
      super()
   }

   leq (ẟ: Delta): boolean {
      return ẟ instanceof New
   }
}

export class Change extends Delta {
   changed: ValueDelta

   constructor (changed: ValueDelta) {
      super()
      this.changed = changed
   }

   leq (ẟ: Delta): boolean {
      return ẟ instanceof Change && leq(this.changed, ẟ.changed)
   }

   hasChanged (prop: string): boolean {
      return fields(this.changed).includes(prop)
   }
}

// Constructor has changed, and therefore fields may not align. More sophisticated reclassification
// delta could allow for fields to be shared when an object changes class.
export class Reclassify extends Delta {
   constructor () {
      super()
   }

   leq (ẟ: Delta): boolean {
      return ẟ instanceof Reclassify
   }
}
