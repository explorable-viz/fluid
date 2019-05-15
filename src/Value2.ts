import { Class, __check, assert } from "./util/Core"

// Use to initialise fields for reflection, without requiring constructors.
export const _: any = undefined 

// Value in the metalanguage.
export abstract class Value<Tag extends string = any> {
   readonly __tag: Tag
}

// Address or location of persistent object.
export abstract class Id extends Value<"Id"> {
}

// Functions are persistent to support primitives. Primitive data types like Num and Str contain
// ES6 primitives like number and string, which are (currently) "persistent" for interning purposes
// but are not "values" because they are not observable to user code.
export type Persistent = Value | string | number | Function

export type PrimValue = Value<"Num"> | Value<"Str">

export class Num extends Value<"Num"> {
   val: number = _
}

export function num (val: number): Num {
   return make(Num, val)
}

export class Str extends Value<"Str"> {
   val: string = _
}

export function str (val: string): Str {
   return make(Str, val)
}

// Tags a value of a datatype constructor; fields are always user-level values (i.e. not ES6 primitives).
export abstract class Constr<Tag extends string = any> extends Value<Tag> {
}

// Dynamic interface to a value object.
export interface State {
   [prop: string]: Persistent
}

// Curried map from constructors and arguments to cached values; curried because composite keys would 
// require either custom equality, which isn't possible with ES6 maps, or interning, which would essentially
// involve the same memoisation logic.
type MemoTable = Map<Persistent, Persistent | Map<Persistent, Object>> // approximate recursive type

// Hash-consed constructors are invariant across worlds, whereas functions are not.
const __ctrMemo: MemoTable = new Map

function lookupArg<T extends Persistent> (f: Memoisable<T>, m: MemoTable, v̅: Persistent[], n: number): Persistent | Map<Persistent, Object> {
   // for memoisation purposes, treat f's key as argument -1
   const k: Persistent = n === -1 ? f.key : v̅[n]
   let v: Persistent | Map<Persistent, Object> | undefined = m.get(k)
   if (v === undefined) {
      if (n === v̅.length - 1) {
         v = f.call(v̅)
         v = v! // TS confused; thinks v can be undefined here
      } else {
         v = new Map
      }
      m.set(k, v)
   }
   return v
}

// Unify memo-functions and interned classes.
interface Memoisable<T extends Persistent> {
   key: Persistent
   call (args: Persistent[]): T
}

class MemoCtr<Tag extends string, T extends Value<Tag>> implements Memoisable<T> {
   C: Class<T>

   constructor (C: Class<T>) {
      this.C = C
   }

   get key (): Persistent {
      return this.C
   } 

   call (v̅: Persistent[]): T {
      const o: T = new this.C
      construct(o, v̅)
      Object.freeze(o) 
      return o
   }
}

export function memoCall<T extends Persistent> (memo: MemoTable, f: Memoisable<T>, v̅: Persistent[]): T {
   let v: Persistent | Map<Persistent, Object> = lookupArg(f, memo, v̅, -1)
   for (let n: number = 0; n < v̅.length; ++n) {
      // since there are more arguments, the last v was a (possibly nested) map
      v = lookupArg(f, v as MemoTable, v̅, n)
   }
   return v as T
}

// Experimented with dictionary-based construction pattern; eliminates field order mismatch as a possible
// source of error, but the benefit is very small and doesn't really suit the memoisation pattern.
export function make<Tag extends string, T extends Value<Tag>> (C: Class<T>, ...v̅: Persistent[]): T {
   return memoCall(__ctrMemo, new MemoCtr(C), v̅)
}

// Depends heavily on (1) getOwnPropertyNames() returning fields in definition-order; and (2)
// constructor functions supplying arguments in the same order.
export function construct<Tag extends string, T extends Value<Tag>> (tgt: T, v̅: Persistent[]): T {
   const tgtʹ: State = tgt as any as State,
         f̅: string[] = fields(tgt)
   assert(f̅.length === v̅.length)
   let n: number = 0
   f̅.forEach((f: string): void => {
      tgtʹ[f] = v̅[n++]
   })
   return tgt
}

// Exclude metadata according to our convention.
export function isField (prop: string): boolean {
   return !prop.startsWith("__")
}

export function fields<Tag extends string> (v: Value<Tag>): string[] {
   return Object.getOwnPropertyNames(v).filter(isField)
}

export function fieldValues (v: Constr): Value[] {
   return fields(v).map(k => (v as any as State)[k] as Value)
}
