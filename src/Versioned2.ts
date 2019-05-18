import { Annotation } from "./util/Annotated2"
import { Class, __nonNull, absurd, assert, className, classOf, notYetImplemented } from "./util/Core"
import { Expl } from "./ExplValue2"
import { Id, Num, Persistent, State, Str, Value, _, construct, fields, make } from "./Value2"

type Expl = Expl.Expl

// Versioned objects are persistent objects that have state that varies across worlds. It doesn't make sense 
// for interned objects to have explanations (or does it?) or annotations. An interface because the same datatype
// can be interned in some contexts and versioned in others.
export interface VersionedValue<Tag extends string, T extends Value<Tag>> extends Value<Tag> {
   __id: Id
   __lastModified: World
   __α?: Annotation        // for some (meta)values this may remain undefined, e.g. tries
   __expl?: Expl           // previously we couldn't put explanations inside values; see GitHub issue #128.
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
      // This may massively suck, performance-wise. Could move to VersionedObject now we have ubiquitous constructors.
      Object.defineProperty(vʹ, "__id", {
         value: k,
         enumerable: false
      })
      __versioned.set(k, vʹ)
      construct(vʹ, v̅)
   } else
   if (v instanceof C) {
      vʹ = construct(v, v̅)
   } else {
      vʹ = reclassify(v, C)
   }
   return commit(vʹ)
}

export function copyAt<Tag extends string, T extends Value<Tag>> (k: Id, v: T): T {
   return at(k, classOf(v), ...v.fieldValues())
}

// A memo key which is sourced externally to the system. (The name "External" exists in the global namespace.)
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

// Call-by-need implementation required LVar-style increasing semantics, but now require equality.
function assertEqualState (tgt: Value, src: Value): void {
   assert(tgt.constructor === src.constructor)
   assert(fields(tgt).length === fields(src).length)
   const src_: State = src as any as State,
         tgt_: State = tgt as any as State
   fields(tgt).forEach((k: string): void => {
      assert(tgt_[k] === src_[k], `Incompatible values for field "${k}"`, tgt, src)
   })
}
   
function commit<Tag extends string, T extends Value<Tag>> (v: T): T {
   const vʹ: VersionedValue<Tag, T> = asVersioned(v)
   if (vʹ.__lastModified === __w) {
      assertEqualState(v, v) // TODO: compare with previous state :)
   } else {
      vʹ.__lastModified = __w
   }
   return v
}
      
export function numʹ (k: Id, val: number): Num {
   return at(k, Num, val)
}

export function strʹ (k: Id, val: string): Str {
   return at(k, Str, val)
}

export class World {
   static revisions: number = 0

   static newRevision (): void {
      console.log(`At revision ${World.revisions++}`)
      __w = new World
   }
}

export let __w: World = new World

export function getα<Tag extends string, T extends Value<Tag>> (v: T): Annotation {
   return __nonNull(asVersioned(v).__α)
}

// TODO: integrate the single-assignment checks below with commit.
export function setα<Tag extends string, T extends Value<Tag>> (α: Annotation, v: T): T {
   const vʹ: VersionedValue<Tag, T> = asVersioned(v)
   if (vʹ.__α === undefined) {
      vʹ.__α = α
   } else {
      if (vʹ.__lastModified === __w) {
         assert(vʹ.__α === α)
      } else {
         vʹ.__lastModified = __w
         vʹ.__α = α
      }
   }
   return v
}

export function copyα<TagU extends string, U extends Value<TagU>, TagT extends string, T extends Value<TagT>> (src: U, v: T): T {
   return setα(getα(src), v)
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
      if (vʹ.__lastModified === __w) {
         assert(vʹ.__expl === t)
      } else {
         vʹ.__lastModified = __w
         vʹ.__expl = t
      }
   }
   return v
}
