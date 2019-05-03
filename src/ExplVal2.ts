import { Class, error } from "./util/Core"

// Value in the metalanguage.
export abstract class Value {
}

export abstract class PrimValue extends Value {
}

export class String extends Value {
   val: string
}

export class Number extends Value {
   val: number
}

// Value of a datatype constructor.
export abstract class Constr<T> extends Value implements Metadata<T> {
   abstract __match<U> (σ: ConstrFunc<U>): U
}

// Func to distinguish from expression-level Fun.
export abstract class Func<T> extends Value {
   abstract __apply (v: Value): T
}

// Should be abstract but currently construct dynamic instances of these.
export class ConstrFunc<T> extends Func<T> {
   __apply (v: Value): T {
      if (v instanceof Constr) {
         return v.__match(this)
         // Less performant but generic alternative:
         // return (this as any as Func_Dyn<T>)[className(v)].__apply(fieldVals(v))
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
   __match<U> (σ: ConstrFunc<U>): U
}  

type State<T> = {
   [prop in Exclude<keyof T, keyof Metadata<T>>]: T[prop] extends Value ? T[prop] : never
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
      abstract __match<U> (σ: ExplFunc<U>): U
   }

   export class Empty extends Expl {
      __match<U> (σ: ExplFunc<U>): U {
         return σ.Empty()
      }
   }

   export function empty (): Empty {
      return make(Empty, {})
   }

   abstract class ExplFunc<U> extends ConstrFunc<U> {
      abstract Empty (): U
   }
}

type Expl = Expl.Expl

export type ExplVal = [Expl, Value]
