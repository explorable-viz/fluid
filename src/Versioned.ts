import { Class, __nonNull, assert } from "./util/Core"
import { __deltas } from "./Delta"
import { Id, Persistent, Num, Str, Value, _, construct, fields, make } from "./Value"

// Versioned objects are persistent objects that have state that varies across worlds. Interface because the 
// same datatype can be interned in some contexts and versioned in others.
export type Versioned<T> = Versioned_ & T

// Why do versioned objects need to store their id?
export interface Versioned_ {
   __id: Id
}

export function versioned <T extends Value> (v: T): boolean {
   return (v as Versioned<T>).__id !== undefined
}

// For versioned objects the map is not curried but takes an (interned) composite key.
type VersionedValues = Map<Id, Versioned<Value>>
const __versioned: VersionedValues = new Map

// The (possibly already extant) versioned object uniquely identified by a memo-key. As an idempotent side-effect,
// record how the object differs from its previous version.
export function at<T extends Value> (C: Class<T>, ...v̅: Persistent[]): (k: Id) => Versioned<T> {
   return (k: Id) => {
      let v: Versioned<Value> | undefined = __versioned.get(k)
      if (v === undefined) {
         const v: Versioned<T> = new C as Versioned<T>
         Object.defineProperty(v, "__id", {
            value: k,
            enumerable: false
         })
         __versioned.set(k, v)
         __deltas.created(v, construct(true, v, v̅)!)
         return v
      } else
      if (v instanceof C) {
         __deltas.changed(v, construct(true, v, v̅)!)
         return v
      } else {
         reclassify(v, C)
         __deltas.reclassified(v, construct(true, v, v̅)!)
         return v as Versioned<T>
      }
   }
}

// Should emulate the post-state of "new C". Probably need to worry about how this works with inherited properties.
function reclassify<T extends Value> (v: Versioned<Value>, ctr: Class<T>): void {
   const proto: Object = Object.getPrototypeOf(new ctr)
   assert (Object.getPrototypeOf(v) !== proto)
   for (const k of fields(v)) {
      assert(delete v[k as keyof Object])
   }
   Object.setPrototypeOf(v, proto)
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

export function num (val: number): (k: Id) => Versioned<Num> {
   return at(Num, val)
}

export function str (val: string): (k: Id) => Versioned<Str> {
   return at(Str, val)
}
