import { Class, absurd } from "./util/Core"
import { Annotation } from "./Annotated2"
import { Expl } from "./ExplValue2"
import { Id, Persistent, Value, _, construct, make } from "./Value2"

type Expl = Expl.Expl

// Versioned objects are persistent objects that have state that varies across worlds. It doesn't make sense 
// for interned objects to have explanations (or does it?) or annotations.
// Interface because the same datatype can be interned in some contexts and versioned in others.
export interface VersionedValue {
   __α?: Annotation  // for some (meta)values this may remain undefined, e.g. tries
   __expl?: Expl     // previously we couldn't put explanations inside values; see GitHub issue #128.
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
type VersionedValues = Map<Id, Value<any>>
const __versioned: VersionedValues = new Map

// The (possibly already extant) versioned object uniquely identified by a memo-key.
export function at<Tag extends string, T extends Value<Tag>> (k: Id, C: Class<T>, ...v̅: Persistent[]): T {
   let v: Value<any> | undefined = __versioned.get(k)
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
      return absurd() // for now
   }
}

// Fresh keys represent inputs to the system, e.g. addresses of syntax nodes provided by an external structure editor.
export const ν: () => Extern =
   (() => {
      let count: number = 0
      return () => {
         return extern(count++)
      }
   })()
