import { Annotation } from "./util/Annotated2"
import { Class, __nonNull, absurd, assert, className, classOf, notYetImplemented } from "./util/Core"
import { Expl } from "./ExplValue2"
import { Id, Num, Persistent, Str, Value, _, construct, make } from "./Value2"

type Expl = Expl.Expl

// Versioned objects are persistent objects that have state that varies across worlds. It doesn't make sense 
// for interned objects to have explanations (or does it?) or annotations.
// Interface because the same datatype can be interned in some contexts and versioned in others.
export interface VersionedValue<Tag extends string, T extends Value<Tag>> extends Value<Tag> {
   __id: Id
   __α?: Annotation  // for some (meta)values this may remain undefined, e.g. tries
   __expl?: Expl     // previously we couldn't put explanations inside values; see GitHub issue #128.
}

export function versioned<Tag extends string, T extends Value<Tag>> (v: Value<Tag>): v is VersionedValue<Tag, T> {
   return (__nonNull(v) as any).__id !== undefined
}

export function asVersioned<Tag extends string, T extends Value<Tag>> (v: T): VersionedValue<Tag, T> {
   if (versioned(v)) {
      return v
   } else {
      return absurd(`Not a versioned value: ${className(v)}`)
   }
}

// A memo key which is sourced externally to the system. (The name "External" exists in the global namespace.)
export class Extern extends Id {
   id: number = _
}

function extern (id: number): Extern {
   return make(Extern, id)
}

// For versioned objects the map is not curried but takes an (interned) composite key. TODO: treating the constructor
// as part of the key isn't correct because objects can change class. To match the formalism, we need a notion of 
// "metatype" or kind, so that traces and values are distinguished, but within those "kinds" the class can change.
type VersionedValues = Map<Id, Value>
const __versioned: VersionedValues = new Map

// The (possibly already extant) versioned object uniquely identified by a memo-key.
export function at<Tag extends string, T extends Value<Tag>> (k: Id, C: Class<T>, ...v̅: Persistent[]): T {
   let v: Value | undefined = __versioned.get(k)
   if (v === undefined) {
      const vʹ: T = new C
      // This may massively suck, performance-wise. Could move to VersionedObject now we have ubiquitous constructors.
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
      return notYetImplemented()
   }
}

export function copyAt<Tag extends string, T extends Value<Tag>> (k: Id, v: T): T {
   return at(k, classOf(v), ...v.fieldValues())
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

// Keep these together for now. TOOD: generalise single-assignment constraint check.

export function getα<Tag extends string, T extends Value<Tag>> (v: T): Annotation {
   return __nonNull(asVersioned(v).__α)
}

export function setα<Tag extends string, T extends Value<Tag>> (α: Annotation, v: T): T {
   const vʹ: VersionedValue<Tag, T> = asVersioned(v)
   if (vʹ.__α === undefined) {
      vʹ.__α = α
   } else {
      assert(vʹ.__α === α)
   }
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

export function setExpl<Tag extends string, T extends Value<Tag>> (t: Expl, v: T): T {
   const vʹ: VersionedValue<Tag, T> = asVersioned(v)
   if (vʹ.__expl === undefined) {
      vʹ.__expl = t
   } else {
      assert(vʹ.__expl === t)
   }
   return v
}
