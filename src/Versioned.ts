import { Annotation, ann } from "./util/Annotated"
import { Class, __nonNull, absurd, className, classOf, notYetImplemented } from "./util/Core"
import { Id, Num, Persistent, Str, Value, ValueTag, _, construct, make, metadataFields } from "./Value"

// For trait idiom see https://www.bryntum.com/blog/the-mixin-pattern-in-typescript-all-you-need-to-know/ and
// https://github.com/Microsoft/TypeScript/issues/21710.
export function AnnotatedC<T extends Class<Value>> (C: T) {
   // https://stackoverflow.com/questions/33605775
   return {
      [C.name]: class extends C {
            __α: Annotation
         }
   }[C.name] // give versioned class same name as C
}

export interface Annotated_ {
   __α: Annotation
}

export type Annotated<T> = Annotated_ & T

export function annotated<T> (v: T): v is Annotated<T> {
   return v.hasOwnProperty("__α")
}

export function asAnnotated<T> (v: T): Annotated<T> {
   if (annotated(v)) {
      return v
   } else {
      return absurd(`Not an annotated value: ${className(v)}`)
   }
}

// Versioned objects are persistent objects that have state that varies across worlds. It doesn't make sense 
// for interned objects to have explanations (or does it?) or annotations. Interface because the same datatype
// can be interned in some contexts and versioned in others.
export type Versioned<T> = Versioned_ & T

export function versioned<T> (v: T): v is Versioned<T> {
   return (v as any).__id !== undefined
}

export function asVersioned<T> (v: T): Versioned<T> {
   if (versioned(v)) {
      return v
   } else {
      return absurd(`Not a versioned value: ${className(v)}`)
   }
}

// Not sure how to avoid duplicating the definitions here.
export interface Versioned_ {
   __id: Id
   __α: Annotation
}

// For versioned objects the map is not curried but takes an (interned) composite key.
type VersionedValues = Map<Id, Versioned<Value>>
const __versioned: VersionedValues = new Map

// The (possibly already extant) versioned object uniquely identified by a memo-key.
export function at<T extends Value> (k: Id, C: Class<T>, ...v̅: Persistent[]): Versioned<T> {
   let v: Versioned<Value> | undefined = __versioned.get(k)
   if (v === undefined) {
      const v: T = new C
      Object.defineProperty(v, "__id", {
         value: k,
         enumerable: false
      })
      const vʹ: Versioned<T> = asVersioned(v)
      __versioned.set(k, vʹ)
      return construct(vʹ, v̅)
   } else
   if (v instanceof C) { 
      return construct(v, v̅) // hmm, TS thinks v is versioned here - why?
   } else {
      return reclassify(v, C)
   }
}

// Make an annotated node, for a class that doesn't already specify statically that its instances are annotated.
export function annotatedAt<T extends Value> (k: Id, C: Class<T>, ...v̅: Persistent[]): Annotated<T> {
   const v: T = at(k, C, ...v̅);
   (v as any).__α = _
   return v as Annotated<T>
}

// Should emulate the post-state of "new C". Probably need to worry about how this works with inherited properties.
function reclassify<T extends Value> (v: Versioned<Value>, ctr: Class<T>): Versioned<T> {
   return notYetImplemented()
}

export function copyAt<T extends Value> (k: Id, v: T): Versioned<T> {
   const vʹ: Versioned<T> = at(k, classOf(v), ...v.fieldValues())
   metadataFields(v).forEach((prop: string) => {
      (vʹ as any)[prop] = (v as any)[prop]
   })
   return vʹ
}

// A memo key which is sourced externally to the system. (The name "External" is already taken.)
export class Extern extends Id {
   id: number = _
}

function extern (id: number): Extern {
   return make(Extern, id)
}

// Fresh keys represent inputs to the system, e.g. addresses of syntax nodes provided by an external structure editor.
export const ν: () => Extern =
   (() => {
      let count: number = 0
      return () => {
         return extern(count++)
      }
   })()

export function num (val: number): Annotated<Num> {
   return annotatedAt(ν(), Num, val)
}

export function str (val: string): Annotated<Str> {
   return annotatedAt(ν(), Str, val)
}

export function setα<T, U extends Annotated<T>> (α: Annotation, v: U): U {
   v.__α = α
   return v
}

export function setallα<Tag extends ValueTag, T extends Value<Tag>> (α: Annotation, v: T): T {
   if (versioned(v)) {
      setα(α, v)
   }
   v.fieldValues().forEach((v: Persistent): void => {
      if (v instanceof Value) {
         setallα(α, v)
      }
   })
   return v
}

export function negateallα<Tag extends ValueTag, T extends Value<Tag>> (v: T): T {
   if (versioned(v)) {
      setα(ann.negate(v.__α), v)
   }
   v.fieldValues().forEach((v: Persistent): void => {
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
