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
