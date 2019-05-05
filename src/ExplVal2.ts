import { Class, className, error } from "./util/Core"
import { fieldVals } from "./DataType2"

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

// Value of a datatype constructor.
export abstract class Constr<T> extends Value {
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

export interface State_Dyn {
   [prop: string]: Persistent
}

export function construct<T extends Value> (tgt: T, state: State_Dyn): T {
   // TODO: copy state to fields of tgt
   return tgt
}

export function make<T extends Value> (ctr: Class<T>, state: State_Dyn): T {
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
