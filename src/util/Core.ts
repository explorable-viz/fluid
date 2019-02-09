import { Eq } from "./Eq"

// A nominal typing idiom; see https://basarat.gitbooks.io/typescript/docs/tips/nominalTyping.html.
export interface Tag<T extends string> {
   typename: T
}

export type Class<T> = new (...args: any[]) => T

// Possibly abstract class; see https://stackoverflow.com/questions/36886082.
export type AClass<T> = Function & { prototype: T }

export function classOf<T> (x: T): AClass<T> {
   return __nonNull(x).constructor as Class<T>
}

export function className(o: Object): string {
   return funName(classOf(o))
}

export function funName<T> (fun: AClass<T>): string {
   return __nonNull(fun.name)
}

export function as<U, T extends U> (x: U, cls: AClass<T>): T {
   if (__nonNull(x) instanceof cls) {
      return <T>x
   } else {
      return assert(false, "[as] Expected " + funName(cls) + ", got " + className(x))
   }
}

export function asOpt<U, T extends U> (x: U, cls: AClass<T>): T {
   if (x === null || x === undefined) {
      return x as T
   } else {
      return as(x, cls)
   }
}

export function assert (b: boolean, msg?: string, ...xs: any[]): any {
   if (!b) {
      if (xs.length > 0) {
         console.warn("Assertion data:\n")
         xs.forEach(x => console.warn(x))
      }
      throw new Error(msg || "Assertion failure")
   }
}

export function absurd (msg?: string, ...xs: any[]): any {
   assert(false, msg, ...xs)
}

// Useful when a notionally abstract class needs to be concrete.
export function abstractMethodError<T> (this_: Object): T {
   return assert(false, "Abstract method in " + this_)
}

export function __nonNull<T> (x: T | null | undefined): T {
   if (x !== null && x !== undefined) {
      return x
   } else {
      return assert(false, "Unexpected null | undefined.")
   }
}

export function __debug (o: Object): string {
   return className(o) + "#" + (<any>o).__id
}

export function __log<T> (
   x: T,
   msg?: (it: T) => string,
   transform: (it: T) => T = (it: T) => it
): T {
   const x_ = transform(x)
   if (msg) {
      console.log(msg(x_))
   }
   console.log(x_)
   return x
}

export function __check<T> (x: T, predicate: (it: T) => boolean): T {
   assert(predicate(x))
   return x
}

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
