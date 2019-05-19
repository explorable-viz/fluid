import { Annotation } from "./util/Annotated2"
import { Class, __nonNull, absurd, className, classOf, notYetImplemented } from "./util/Core"
import { Expl } from "./ExplValue2"
import { Id, Num, Persistent, Str, Value, _, construct, make } from "./Value2"

type Expl = Expl.Expl

// Versioned objects are persistent objects that have state that varies across worlds. It doesn't make sense 
// for interned objects to have explanations (or does it?) or annotations. Interface because the same datatype
// can be interned in some contexts and versioned in others.
export interface VersionedValue extends Value {
   __id: Id
   __α: Annotation        
   __expl: Expl     // previously we couldn't put explanations inside values; see GitHub issue #128.
}

// For idiom and usage see https://www.bryntum.com/blog/the-mixin-pattern-in-typescript-all-you-need-to-know/ and
// https://github.com/Microsoft/TypeScript/issues/21710.
export function Versioned<T extends Class<Value>> (C: T) {
   class VersionedC extends C {
      __id: Id
      __α: Annotation
      __expl: Expl
   }
   return VersionedC
}

export type Versioned<Tag extends string, T extends Value<Tag>> = VersionedValue & Value<Tag>

export function versioned<Tag extends string, T extends Value<Tag>> (v: Value<Tag>): v is VersionedValue {
   return (__nonNull(v) as any).__id !== undefined
}

export function asVersioned<Tag extends string, T extends Value<Tag>> (v: T): Versioned<Tag, T> {
   if (versioned(v)) {
      return v
   } else {
      return absurd(`Not a versioned value: ${className(v)}`)
   }
}

// Should emulate the post-state of "new C". Probably need to worry about how this works with inherited properties.
function reclassify<Tag extends string, T extends Value<Tag>> (v: Value, ctr: Class<T>): T {
   return notYetImplemented()
}

// For versioned objects the map is not curried but takes an (interned) composite key.
type VersionedValues = Map<Id, Value>
const __versioned: VersionedValues = new Map

// The (possibly already extant) versioned object uniquely identified by a memo-key.
export function at<Tag extends string, T extends Value<Tag>> (k: Id, C: Class<T>, ...v̅: Persistent[]): T {
   let v: Value | undefined = __versioned.get(k)
   let vʹ: T
   if (v === undefined) {
      vʹ = new C
      // Not sure of performance implications, or whether enumerability of __id matters much.
      Object.defineProperty(vʹ, "__id", {
         value: k,
         enumerable: false
      })
      __versioned.set(k, vʹ)
      return construct(vʹ, v̅)
   } else
   if (v instanceof C) {
      return construct(v, v̅)
   } else {
      return reclassify(v, C)
   }
}

export function copyAt<Tag extends string, T extends Value<Tag>> (k: Id, v: T): T {
   return at(k, classOf(v), ...v.fieldValues())
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

export function numʹ (k: Id, val: number): Num {
   return at(k, Num, val)
}

export function strʹ (k: Id, val: string): Str {
   return at(k, Str, val)
}

export function getα<Tag extends string, T extends Value<Tag>> (v: T): Annotation {
   return __nonNull(asVersioned(v).__α)
}

export function setα<Tag extends string, T extends Value<Tag>> (α: Annotation, v: T): T {
   asVersioned(v).__α = α
   return v
}

export function setallα<Tag extends string, T extends Value<Tag>> (v: T, α: Annotation): T {
   if (versioned(v)) {
      setα(α, v)
   }
   v.fieldValues().forEach((v: Persistent): void => {
      if (v instanceof Value) {
         setallα(v, α) 
      }
   })
   return v
}

export function getExpl<Tag extends string, T extends Value<Tag>> (v: T): Expl {
   return __nonNull(asVersioned(v).__expl)
}

export function setExpl<Tag extends string, T extends Value<Tag>> (t: Expl, v: T): T {
   asVersioned(v).__expl = t
   return v
}
