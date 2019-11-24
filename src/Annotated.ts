import { Class, __nonNull, absurd } from "./util/Core"
import { Annotation, bool_ } from "./util/Lattice"
import { __deltas } from "./Delta" 
import { Persistent, Value, ValueTag, _ } from "./Value"
import { MemoFunType, memo } from "./Versioned"

// For trait idiom see https://www.bryntum.com/blog/the-mixin-pattern-in-typescript-all-you-need-to-know/ and
// https://github.com/Microsoft/TypeScript/issues/21710.
export function AnnotatedC<T extends Class<Value>> (C: T) {
   // https://stackoverflow.com/questions/33605775
   return {
      [C.name]: class extends C {
            __α: Annotation = _
         }
   }[C.name] // give versioned class same name as C
}

export interface Annotated {
}

export function annotated<T extends Object> (v: T): v is T & Annotated {
   return v.hasOwnProperty("__α")
}

export function getα<T extends Annotated & Value> (v: T): Annotation {
   return __annotations.get(v)
}

export function setα<T extends Annotated & Value> (α: Annotation, v: T): T {
   if (getα(v) !== α) {
      __deltas.changed(v, { __α: { before: getα(v), after: α } })
   }
   __annotations.set(v, α)
   return v
}

// Memoising an imperative function makes any side effects idempotent. Not clear yet how to "partially" 
// memoise LVar-like functions like joinα, but setall isn't one of those.
export function setallα<T extends Persistent> (α: Annotation, v: T): T {	
   return memo<T>(setallα_ as MemoFunType<T>, α, v)
}	

export function setallα_<Tag extends ValueTag, T extends Value<Tag>> (α: Annotation, v: T): T {
   return v
}

export function negateallα<T extends Persistent> (v: T): T {	
   return memo<T>(negateallα_ as MemoFunType<T>, v)
}	

export function negateallα_<Tag extends ValueTag, T extends Value<Tag>> (v: T): T {
   if (annotated(v)) {
      setα(bool_.negate(getα(v)), v)
   }
   v.__children.forEach((v: Persistent): void => {
      if (v instanceof Value) {
         negateallα(v)
      }
   })
   return v
}

export function setjoinα<T extends Annotated & Value> (α: Annotation, v: T): T {
   return setα(bool_.join(α, getα(v)), v)
}

export function setmeetα<T extends Annotated & Value> (α: Annotation, v: T): T {
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
