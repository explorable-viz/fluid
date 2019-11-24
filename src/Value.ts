import { Class, __nonNull, absurd, assert } from "./util/Core"
import { Ord } from "./util/Ord"
import { ValueDelta, __deltas } from "./Delta"

// Use to initialise fields for reflection, without requiring constructors.
export const _: any = undefined 

// Somewhat perverse to do this, but need some type safety!
export type DataValueTag =
   // TODO: sync these up when new graphics design complete.
   "Graphic" | "Viewport" | "Circle" | "Group" | "Line" | "Marker" | "Orient" | "Polyline" | "Polymarkers" | "Polygon" | "Scale" | "Text" | "Transform" | "LeftTick" | "RightTick" | "Circle" | "Arrowhead" |
   "Bool" | "Closure" | "DataExpl" | "Elim" | "Match" | "Env" | "Expl" | "Expl.Def" | "Expl.RecDef" | "ExplValue" | "Expr" | "Expr.Def" |  
   "Translate" | "List" | "Option" | "Ordering" | "Pair" | "Plug" | "Point" | "RecDef" | "Rect" | "Tree" | "Token"
export type LexemeTag = "Whitespace" | "SingleLineComment" | "Operator"
export type PrimOpTag = "UnaryOp" | "BinaryOp"
export type ValueTag = DataValueTag | LexemeTag | PrimOpTag | "Id" | "Num" | "Str"

// Value in the metalanguage.
export class Value<Tag extends ValueTag = ValueTag> {
   readonly __tag!: Tag

   __child (k: keyof this): Persistent {
      return (this as any)[k]
   } 

   // Probably confusingly, "children" isn't a user-level notion; specifically, wrappers
   // like Num and Str have children which are not observable through pattern-matching.
   get __children (): Persistent[] {
      return fields(this).map(k => this.__child(k))
   }
}

// Address or location of persistent object.
export abstract class Id extends Value<"Id"> {
   tag<Tag extends string> (tag: Tag): TaggedId<Tag> {
      return taggedId(this, tag)
   }
}

class FunctionId extends Id {
   f: Function = _
}

function functionId (f: Function): FunctionId {
   return make(FunctionId, f)
}

export class ApplicationId extends Id {
   k: MemoId = _
   v: Persistent = _
}

export type MemoId = FunctionId | ApplicationId

function applicationId (k: MemoId, v: Persistent): ApplicationId {
   return make(ApplicationId, k, v)
}

export class TaggedId<Tag extends string> extends Id {
   k: Id = _
   prop: Tag = _
}

function taggedId<Tag extends string> (k: Id, prop: Tag): TaggedId<Tag> {
   return make(TaggedId, k, prop) as TaggedId<Tag>
}

export function memoId (f: Function, v̅: Iterable<any>): MemoId {
   const fʹ: FunctionId = functionId(f)
   let k: MemoId = fʹ
   for (let v of v̅) {
      k = applicationId(k, v)
   }
   return k
}

// Functions are persistent to support primitives. Primitive datatypes like Num and Str contain
// ES6 primitives like number and string, which are (currently) "persistent" for interning purposes
// but are not "values" because they are not observable to user code. Booleans are persistent
// to support annotation helpers.
export type Persistent = Value | boolean | string | number | Function

export type PrimValue = Num | Str

export function isPrim (v: Value): boolean {
   return v instanceof Num || v instanceof Str
}

export class Num extends Value<"Num"> {
   val: number = _

   toString (): string {
      return this.val.toString()
   }
}

export class Str extends Value<"Str"> implements Ord<Str> {
   val: string = _

   toString (): string {
      return `"${this.val}"`
   }

   leq (str: Str): boolean {
      return this.val.localeCompare(str.val) <= 0
   }

   eq (str: Str): boolean {
      return this.val.localeCompare(str.val) === 0
   }

   geq (str: Str): boolean {
      return this.val.localeCompare(str.val) >= 0
   }
}

// Mergeable state deltas are disjoint.
export function mergeInto (tgt: ValueDelta, src: ValueDelta): void {
   Object.keys(src).forEach((prop: string): void => {
      if (!tgt.hasOwnProperty(prop)) {
         tgt[prop] = src[prop]
      } else {
         absurd(
            `Incompatible update of field "${prop}" at revision.`,
            tgt[prop], 
            src[prop]
         )
      }
   })
}

// Curried map from constructors and arguments to cached values; curried because composite keys would 
// require either custom equality, which isn't possible with ES6 maps, or interning, which would essentially
// involve the same memoisation logic.
export type MemoTable = Map<Persistent, Persistent | Map<Persistent, Object>> // approximate recursive type

// Hash-consed constructors are invariant across worlds.
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
export interface Memoisable<T extends Persistent> {
   key: Persistent
   call (args: Persistent[]): T
}

class MemoCtr<T extends Value> implements Memoisable<T> {
   C: Class<T>

   constructor (C: Class<T>) {
      this.C = C
   }

   get key (): Persistent {
      return this.C
   } 

   call (v̅: Persistent[]): T {
      const v: T = new this.C
      construct(false, v, v̅)
      Object.freeze(v)
      return v
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
export function make<T extends Value> (C: Class<T>, ...v̅: Persistent[]): T {
   return memoCall(__ctrMemo, new MemoCtr(C), v̅)
}

// Depends heavily on (1) getOwnPropertyNames() returning fields in definition-order; and (2)
// constructor functions supplying arguments in the same order.
export function construct<T extends Value> (compare: boolean, tgt: T, v̅: Persistent[]): ValueDelta | null {
   const f̅: (keyof T)[] = fields(tgt),
         ẟ: ValueDelta | null = compare ? {} : null
   assert(f̅.length === v̅.length)
   let n: number = 0
   f̅.forEach((prop: keyof T): void => {
      const src: Persistent = v̅[n++]
      if (compare && tgt.__child(prop) !== src) {
         ẟ![prop as string] = { before: tgt.__child(prop), after: src }
      }
      (tgt as any)[prop] = src
   })
   return ẟ
}

// Exclude metadata according to our convention.
export function isField (prop: string): boolean {
   return !prop.startsWith("__")
}

export function fields<T> (v: T): (keyof T)[] {
   return Object.getOwnPropertyNames(v).filter(isField) as (keyof T)[]
}

export function metadataFields (v: Value): string[] {
   return Object.getOwnPropertyNames(v).filter(f => !isField(f) && f !== "__id")
}
