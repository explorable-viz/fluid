import { Class, className, error } from "./util/Core"
import { fieldVals } from "./DataType2"

// Value in the metalanguage. TODO: rename to PersistentObj?
export abstract class Value {
}

type Persistent = Value | string | number

export class Num extends Value {
   val: number
}

export function num (val: number): Num {
   return make(Num, val)
}

export class Str extends Value {
   val: string
}

export function str (val: string): Num {
   const blah: Persistent = val
   return make(Str, blah)
}

// Value of a datatype constructor.
export abstract class Constr<T> extends Value implements Metadata<T> {
   __match<U> (σ: ConstrFunc<U>): U {
      return σ.__apply(this)
   }
}

// Func to distinguish from expression-level Fun.
export abstract class Func<T> extends Value {
   abstract __apply (v: Value): T
}

export class ConstrFunc<T> extends Func<T> {
   __apply (v: Value): T {
      if (v instanceof Constr) {
         // Probably slow compared to visitor pattern :-o
         return (this as any as Func_Dyn<T>)[className(v)].__apply(fieldVals(v))
      } else {
         return error("Not a datatype")
      }
   }
}

export abstract class ArgumentsFunc<T> extends Value {
   abstract __apply (v̅: Value[]): T
}

// Can't add __apply to this because inconsistent with index signature.
export interface Func_Dyn<T> {
   [ctr: string]: ArgumentsFunc<T>
}

// Dynamic version of State?
export interface State_Dyn {
   [prop: string]: Value
}

type ExplState<T> = { 
   [prop in keyof State<T>]: Expl 
}

// Gather the metadata properties associated with T. The __ prefix indicates these properties must not be treated as "data fields";
// statically, we can express that by excluding all properties which are properties of Metadata, but dynamically there isn't an easy way
// to express that.
interface Metadata<T> {
   __expl?: ExplState<T>
}  

type State<T> = {
   [prop in Exclude<keyof T, keyof Metadata<T>>]: T[prop] extends Persistent ? T[prop] : never
}

// Not easy to put this into Explainable and have it be specifically typed enough.
export function construct<T extends Value> (tgt: T, state: State<T>): T {
   return construct_dyn(tgt, state) as T
}

// Dynamic version of construct.
function construct_dyn (tgt: Value, state: State_Dyn): Value {
   // TODO: copy state to fields of tgt
   return tgt
}

export function make<T extends Value> (ctr: Class<T>, state: State<T>): T {
   return construct(new ctr, state)
}

export namespace Expl {
   export abstract class Expl extends Value {
   }

   export class Empty extends Expl {
   }

   export function empty (): Empty {
      return make(Empty, {})
   }
}

type Expl = Expl.Expl

export type ExplVal = [Expl, Value]
