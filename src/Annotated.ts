import { Class, __nonNull } from "./util/Core"
import { Annotation, ann } from "./util/Lattice"
import { MemoFunType, Persistent, Value, ValueTag, _, setDelta, memo } from "./Value"

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
   __α: Annotation
}

export function annotated<T extends Object> (v: T): v is T & Annotated {
   return v.hasOwnProperty("__α")
}

export function setα<T extends Annotated & Value> (α: Annotation, v: T): T {
   if (v.__α !== α) {
      setDelta(v, "__α", α)
   }
   v.__α = α
   return v
}

// Memoising an imperative function makes any side effects idempotent. Not clear yet how to "partially" 
// memoise LVar-like functions like joinα, but setall isn't one of those.
export function setallα<T extends Persistent> (α: Annotation, v: T): T {	
   return memo<T>(setallα_ as MemoFunType<T>, α, v)
}	

export function setallα_<Tag extends ValueTag, T extends Value<Tag>> (α: Annotation, v: T): T {
   if (annotated(v)) {
      setα(α, v)
   }
   v.children().forEach((v: Persistent): void => {
      if (v instanceof Value) {
         setallα(α, v)
      }
   })
   return v
}

export function negateallα<T extends Persistent> (v: T): T {	
   return memo<T>(negateallα_ as MemoFunType<T>, v)
}	

export function negateallα_<Tag extends ValueTag, T extends Value<Tag>> (v: T): T {
   if (annotated(v)) {
      setα(ann.negate(v.__α), v)
   }
   v.children().forEach((v: Persistent): void => {
      if (v instanceof Value) {
         negateallα(v)
      }
   })
   return v
}

export function setjoinα<T extends Annotated & Value> (α: Annotation, v: T): T {
   return setα(ann.join(α, v.__α), v)
}

export function setmeetα<T extends Annotated & Value> (α: Annotation, v: T): T {
   return setα(ann.meet(α, v.__α), v)
}
