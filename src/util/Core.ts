export type Class<T> = new (...args: any[]) => T

// Possibly abstract class; see https://stackoverflow.com/questions/36886082.
export type AClass<T> = Function & { prototype: T }

export function classOf<T>(x: T): AClass<T> {
   return x.constructor as Class<T>
}

export function className(o: Object): string {
   return funName(classOf(o))
}

export function funName<T>(fun: AClass<T>): string {
   return fun.name
}

export function as<U, T extends U>(x: U, cls: AClass<T>): T {
   if (x !== null && x === undefined) {
      return <T>x // safe for null or undefined
   } else
   if (x instanceof cls) {
      return <T>x
   } else {
      return assert(false, "[as] Expected " + funName(cls) + ", got " + className(x))
   }
}

export function assert(b: boolean, msg?: string, ...xs: any[]): any {
   if (!b) {
      if (xs.length > 0) {
         console.warn("Assertion data:\n")
         xs.forEach(x => console.warn(x))
      }
      throw new Error(msg || "Assertion failure")
   }
}

// Useful when a notionally abstract classes needs to be concrete.
export function abstractMethodError<T>(this_: Object): T {
   return assert(false, "Abstract method in " + this_)
}

export function __nonNull<T>(x: T): T {
   assert(x !== null && x !== undefined)
   return x
}

export function __debug(o: Object): string {
   return className(o) + "#" + (<any>o).__id
}

export function __log<T>(
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

// Curried map from constructors and arguments to constructed objects.
var __instances: Map<any, Object> = new Map()

// For memoisation purposes, treat the constructor itself as argument -1.
function lookupArg(
   ctr: new (...args: any[]) => Object,
   m: Map<any, Object>,
   args: any[],
   n: number
): Object {
   const k = n === -1 ? ctr : args[n]
   let v = m.get(k)
   if (v === undefined) {
      if (n === args.length - 1) {
         v = new ctr()
         ctr.apply(v, args)
      } else {
         v = new Map()
      }
      m.set(k, v)
   }
   return v
}

// For memoisation purposes, treat the function itself as argument -1.
// Functionality of f hard to assert because of allocation of new objects.
function lookupArg_(
   f: (...args: any[]) => any,
   m: Map<any, any>,
   args: any[],
   n: number
): any {
   const k = n === -1 ? f : args[n]
   let v = m.get(k)
   if (v === undefined) {
      if (n === args.length - 1) {
         v = f.apply(args[0], args.slice(1))
      } else {
         v = new Map()
      }
      m.set(k, v)
   }
   return v
}

// Hash-consing (interning) object construction.
export function make<T extends Object>(ctr: Class<T>, ...args: any[]): T {
   let v: Object = lookupArg(ctr, __instances, args, -1)
   for (var n: number = 0; n < args.length; ++n) {
      // since there are more arguments, the last v was a (nested) map
      v = lookupArg(ctr, v as Map<any, Object>, args, n)
   }
   return v as T
}

// Memoisation. TODO: reimplement hash-consing using this.
export function memo<T>(f: (...args: any[]) => T, ...args: any[]): T {
   var v: any = lookupArg_(f, __instances, args, -1)
   for (var n: number = 0; n < args.length; ++n) {
      // since there are more arguments, the last v was a (nested) map
      v = lookupArg_(f, <Map<any, any>>v, args, n)
   }
   return <T>v
}
