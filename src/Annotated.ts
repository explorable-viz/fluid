import { Class, absurd, className } from "./util/Core"
import { Annotation, ann } from "./util/Lattice"
import { Id, MemoFunType, Num, Persistent, Str, Value, ValueTag, _, memo } from "./Value"
import { ν, at } from "./Versioned"

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

// Not sure how to avoid duplicating the body with those of the classes returned by AnnotatedC.
export interface Annotated_ {
   __α: Annotation
}

export type Annotated<T> = Annotated_ & T

export function annotated<T extends Object> (v: T): v is Annotated<T> {
   return v.hasOwnProperty("__α")
}

export function asAnnotated<T> (v: T): Annotated<T> {
   if (annotated(v)) {
      return v
   } else {
      return absurd(`Not an annotated value: ${className(v)}`)
   }
}

export function setα<T, U extends Annotated<T>> (α: Annotation, v: U): U {
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

export function negateallα<Tag extends ValueTag, T extends Value<Tag>> (v: T): T {
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

export function joinα<T, U extends Annotated<T>> (α: Annotation, v: U): U {
   v.__α = ann.join(α, v.__α)
   return v
}

export function meetα<T, U extends Annotated<T>> (α: Annotation, v: U): U {
   v.__α = ann.meet(α, v.__α)
   return v
}

// Make an annotated node, for a class that doesn't already specify statically that its instances are annotated.
export function annotatedAt<T extends Value> (k: Id, C: Class<T>, ...v̅: Persistent[]): Annotated<T> {
   const v: T = at(k, C, ...v̅);
   (v as any).__α = _
   return v as Annotated<T>
}

export function num (val: number): Annotated<Num> {
   return annotatedAt(ν(), Num, val)
}

export function str (val: string): Annotated<Str> {
   return annotatedAt(ν(), Str, val)
}
