import { Class, __nonNull, notYetImplemented } from "./util/Core"
import { Id, Persistent, Num, Str, Value, _, construct, make } from "./Value"

// Versioned objects are persistent objects that have state that varies across worlds. Interface because the 
// same datatype can be interned in some contexts and versioned in others.
export type Versioned<T> = Versioned_ & T

export interface Versioned_ {
   __id: Id
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
      const vʹ: Versioned<T> = v as Versioned<T>
      __versioned.set(k, vʹ)
      return construct(vʹ, v̅)
   } else
   if (v instanceof C) { 
      return construct(v, v̅) // hmm, TS thinks v is versioned here - why?
   } else {
      return reclassify(v, C)
   }
}

export function at_<T extends Value> (C: Class<T>, ...v̅: Persistent[]): (k: Id) => Versioned<T> {
   return (k: Id) => at(k, C, ...v̅)
}

// Should emulate the post-state of "new C". Probably need to worry about how this works with inherited properties.
function reclassify<T extends Value> (v: Versioned<Value>, ctr: Class<T>): Versioned<T> {
   return notYetImplemented()
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
   return at_(Num, val)
}

export function str (val: string): (k: Id) => Versioned<Str> {
   return at_(Str, val)
}
