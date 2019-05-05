import { Class } from "./util/Core"
import { UnaryOp } from "./Primitive2"

// use to initialise fields for reflection, without requiring constructors
export const _: any = undefined 

// Value in the metalanguage. TODO: rename to PersistentObj?
export abstract class Value {
}

// Functions are persistent to support primitives.
type Persistent = Value | string | number | Function

export class Num extends Value {
   val: number
}

export function num (val: number): Num {
   return make(Num, { val })
}

export class Str extends Value {
   val: string
}

export function str (val: string): Str {
   return make(Str, { val })
}

// Tags a value of a datatype constructor.
export abstract class Constr<T> extends Value {
}

export class PrimOp extends Value {
   op: UnaryOp
}

export function primOp (op: UnaryOp): PrimOp {
   return make(PrimOp, { op })
}

// Dynamic interface to a value object.
export interface State {
   [prop: string]: Persistent
}

export function construct<T extends Value> (tgt: T, state: State): T {
   // TODO: copy state to fields of tgt
   return tgt
}

export function make<T extends Value> (ctr: Class<T>, state: State): T {
   return construct(new ctr, state)
}
