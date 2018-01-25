// Probably embodies some assumptions about how TypeScript compiles to JavaScript. I want to access the
// name property of that function, but that's not exposed via the TypeScript type system. Possibly not
// portable either. TODO: would like to give the argument a constructor type, but unfortunately that
// doesn't work with Number or String.
export function className (d: any): string {
   return d.name
}

// Used to be able to type "fun" as (...xs: any[]) => any, but that doesn't seem to work on on the compiler
// development branch any more.
export function funName (fun: Function): string {
   return (<any>fun).name // Cast up to access "name" property. Wtf?
}

// See 0.3 release notes. It used to be neat the way I could supply a class as a first-class value to this
// function, and obtain the static type of its instances at the same time. This no longer seems to work;
// I can't give the "type" argument the constructor signature new (...args: any[]) => a.
export function typeCheck <a> (v: a, type: Function): a {
   if (v === null) // || v === undefined)
      return v
   assertMessage(v instanceof type, '[typeCheck] Expected ' + className(type), v)
   return v
}

export function assert (b: boolean): any {
   assertMessage(b, 'Assertion failure.')
}

export function assertMessage (b: boolean, msg: string, ...xs: any[]): any {
   if (!b) {
      failWithMessage(msg, ...xs)
   }
}

export function failWithMessage(msg: string, ...xs: any[]): never {
   console.log('ABORTING.')
   for (var i: number = 0; i < xs.length; ++i)
      console.log(xs[i])
   throw new Error(msg)
}

export function notImplementedException (this_: Object): any {
   assertMessage(false, 'Unimplemented method in ' + this_)
}

// Mark any variable as unused (for use with --noUnusedLocals).
export function __unused(x: any): void {
}

// Faster than e.g. counting the number of properties and testing against zero. Need to use
// hasOwnProperty if I want this to work in the presence of Object.prototype being modified, but
// for now I can assume not.
export function isEmpty (o: Object): boolean {
   for (let key in o) {
      __unused(key)
      return false
   }
   return true
}

// Maybe there's a library somewhere that does this. Function literals are not values at the top-level
// in JavaScript, but more like statements. If I want a first-class value, I have to jump through a hoop.
export function makeFun (def: string) {
   return eval('(function dummy () { return ' + def + '})()')
}

// Debug helper for diagnosing property access of null or undefined.
export function __nonNull <T> (x: T | null | undefined): T {
   if (x === null || x === undefined) {
      return failWithMessage("Unexpected null or undefined.")
   } else {
      return x
   }
}

export function nop (): void {
}

export interface ArrayConstructor<T> {
   zip <U> (xs: T[], ys: U[]): [T, U][]
}

// TODO: fix interface above so I don't need this cast.
(<any>Array).zip = function <T, U> (xs: T[], ys: U[]): [T, U][] {
   return xs.map((x: T, i: number): [T, U] => {
      return [x, ys[i]]
   })
}
