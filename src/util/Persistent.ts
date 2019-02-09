import { Class } from "./Core"
import { Eq } from "./Eq"

// An object which can be used as a key in an ES6 map (i.e. one for which equality is ===). In particular
// interned objects are persistent objects.
export abstract class PersistentObject implements Eq<PersistentObject> {
   __tag: "PersistentObject"

   // The implementations of these are all identical but this forces a concrete partitioning.
   abstract eq (that: PersistentObject): boolean
}

// Functions are persistent to support primitives.
export type Persistent = null | PersistentObject | string | number | Function

// Curried map from constructors and arguments to constructed objects; curried because composite keys would 
// require either custom equality, which isn't possible with ES6 maps, or interning, which would essentially
// involve the same memoisation logic.
const __instances: Map<Persistent, Object> = new Map()

function lookupArg (
   ctr: new (...args: Persistent[]) => Object,
   m: Map<Persistent, Object>,
   args: Persistent[],
   n: number
): Object {
   // for memoisation purposes, treat constructor itself as argument -1
   const k: Persistent = n === -1 ? ctr : args[n]
   let v: Object | undefined = m.get(k)
   if (v === undefined) {
      if (n === args.length - 1) {
      v = new ctr(...args)
      } else {
         v = new Map()
      }
      m.set(k, v)
   }
   return v
}

// Hash-consing (interning) object construction.
export function make<T extends PersistentObject> (ctr: Class<T>, ...args: Persistent[]): T {
   let v: Object = lookupArg(ctr, __instances, args, -1)
   for (var n: number = 0; n < args.length; ++n) {
      // since there are more arguments, the last v was a (nested) map
      v = lookupArg(ctr, v as Map<Persistent, Object>, args, n)
   }
   Object.freeze(v)
   return v as T
}
