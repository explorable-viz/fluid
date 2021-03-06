import { __nonNull, absurd } from "./util/Core"
import { Annotation, bool_ } from "./util/Lattice"
import { intersection, union } from "./util/Set"
import { DataValue, ExplValue } from "./DataValue"
import { __deltas } from "./Delta"
import { Expl } from "./Expl"
import { Expr } from "./Expr"
import { Value, _ } from "./Value"

export type Annotated = Expr.SyntaxNode | ExplValue
export type Slice = Set<Annotated>

export function annotated (v: Value): v is Annotated {
   return v instanceof Expr.SyntaxNode || v instanceof ExplValue
}

export function isα (v: Annotated): Annotation {
   return __slice.is(v)
}

// Currently no deltas are associated with annotations.
export function setα<T extends Annotated> (α: Annotation, v: T): void {
   __slice.set(v, α)
}

export function setjoinα (α: Annotation, v: Annotated): void {
   setα(bool_.join(α, isα(v)), v)
}

export function setmeetα (α: Annotation, v: Annotated): void {
   setα(bool_.meet(α, isα(v)), v)
}

export enum Direction { Fwd, Bwd }

export class Annotations {
   ann: Slice = new Set() // unavailable nodes (fwd) or needed nodes (bwd)
   direction: Direction = Direction.Fwd

   // Whether v is needed (going backward) or available (going forward).
   is (v: Annotated): Annotation {
      if (this.direction === Direction.Fwd) {
         return bool_.negate(this.ann.has(v))
      } else {
         return this.ann.has(v)
      }
   }
   
   // Going forward, annotation updates must be decreasing; going backward, increasing. This is because 
   // forward slicing propagates non-availability, whereas backward slicing propagates demand.
   set (v: Annotated, α: Annotation): void {
      const current: Annotation = this.is(v)
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
      this.ann = new Set()
   }

   restrictTo (tvs: ExplValue[]): Slice {
      return intersection(this.ann, union(...tvs.map(tv => explDescendants(tv))))
   }
}

function explDescendants (tv: ExplValue): Set<ExplValue> {
   const desc: Set<ExplValue> = new Set()
   explDescendants_aux(tv, desc)
   return desc
}

function explDescendants_aux (tv: ExplValue, desc: Set<ExplValue>): void {
   desc.add(tv)
   if (tv.v instanceof DataValue) {
      const {t, v}: ExplValue<DataValue> = tv as ExplValue<DataValue>
      Expl.explChildren(t,v).forEach((tv: ExplValue): void => { 
         explDescendants_aux(tv, desc)
      })
   }
}

export const __slice: Annotations = new Annotations()
