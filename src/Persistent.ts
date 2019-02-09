import { Class } from "./util/Core"
import { Eq } from "./util/Eq"

// An object which can be used as a key in an ES6 map (i.e. one for which equality is ===). In particular
// interned objects are persistent objects.
export abstract class PersistentObject implements Eq<PersistentObject> {
   __tag: "PersistentObject"

   // The implementations of these are all identical but this forces a concrete partitioning.
   abstract eq (that: PersistentObject): boolean
}

export type Persistent = null | PersistentObject | string | number

// Tag class that identifies dynamically that an object has a structural notion of equality (i.e. is not
// persistent).
export abstract class ValueObject implements Eq<ValueObject> {
   abstract eq (o: ValueObject): boolean
}

// Curried map from constructors and arguments to constructed objects; curried because composite keys would 
// require either custom equality, which isn't possible with ES6 maps, or interning, which would essentially
// involve the same memoisation logic.
const __instances: Map<any, Object> = new Map()

// For memoisation purposes, treat the constructor itself as argument -1.
function lookupArg (
   ctr: new (...args: any[]) => Object,
   m: Map<any, Object>,
   args: any[],
   n: number
): Object {
   const k = n === -1 ? ctr : args[n]
   let v = m.get(k)
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

// Hash-consing (interning) object construction. TODO: replace "any" by Persistent?
export function make<T extends PersistentObject> (ctr: Class<T>, ...args: any[]): T {
   let v: Object = lookupArg(ctr, __instances, args, -1)
   for (var n: number = 0; n < args.length; ++n) {
      // since there are more arguments, the last v was a (nested) map
      v = lookupArg(ctr, v as Map<any, Object>, args, n)
   }
   Object.freeze(v)
   return v as T
}
