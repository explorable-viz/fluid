import { Class } from "./util/Core"

// Value in the metalanguage.
export abstract class Value {
   abstract __match<U> (σ: Func<U>): U
}

export abstract class Explainable<T> extends Value implements Metadata<T> {
   __expl?: ExplState<T>
}

// Called Func to avoid confusion with expression-level Fun.
export abstract class Func<T> {
   __apply (v: Value): T {
      return v.__match(this)
   }
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
   __match<U> (σ: Func<U>): U
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

   abstract class ExplFunc<U> extends Func<U> {
      abstract Empty (): U
   }
}

type Expl = Expl.Expl

export type ExplVal = [Expl, Value]
