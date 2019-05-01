import { Class } from "./util/Core"

// Value in the metalanguage.
export abstract class Value {
}

export abstract class Explainable<T> extends Value implements Metadata<T> {
   __expl?: ExplState<T>
   abstract __match<U> (σ: Fun<U>): U
}

export class Fun<U> extends Value {
}

type State<T> = CoreProps<T>

// Dynamic version of State?
export interface State_Dyn {
   [prop: string]: Value
}

type ExplState<T> = { 
   [prop in keyof CoreProps<T>]: Expl 
}

// Gather the metadata properties associated with T. The __ prefix indicates these properties must not be treated as "data fields";
// statically, we can express that by excluding all properties which are properties of Metadata, but dynamically there isn't an easy way
// to express that.
interface Metadata<T> {
   __expl?: ExplState<T>
   __match (σ: T): void
}  

type CoreProps<T> = Pick<T, Exclude<keyof T, keyof Metadata<T>>>

// Not easy to put this into Explainable and have it be specifically typed enough.
export function construct<T> (tgt: T, state: State<T>): T {
   return construct_dyn(tgt, state) as T
}

// Dynamic version of construct.
function construct_dyn (tgt: Value, state: State_Dyn): Value {
   // TODO: copy state to fields of tgt
   return tgt
}

export function make<T> (ctr: Class<T>, state: State<T>): T {
   return construct(new ctr, state)
}

export namespace Expl {
   export class Expl {
   }

   export class Empty extends Expl {
   }

   export function empty (): Empty {
      return make(Empty, {})
   }
}

type Expl = Expl.Expl

export type ExplVal = [Expl, Value]
