import { Class } from "./util/Core"

// Value in the metalanguage.
export abstract class Value {
}

export abstract class Explainable<T> extends Value implements Metadata<T> {
   expl?: ExplState<T>
   abstract match<U> (σ: Fun<U>): U
}

export class Fun<U> extends Value {
}

type State<T> = CoreProps<T>

// Dynamic version of State?
export interface Stateʹ {
   [prop: string]: Value
}

type ExplState<T> = { 
   [prop in keyof CoreProps<T>]: Expl 
}

// Gather the metadata properties associated with T.
interface Metadata<T> {
   expl?: ExplState<T>
   match (σ: T): void
}  

type CoreProps<T> = Pick<T, Exclude<keyof T, keyof Metadata<T>>>

// Not easy to put this into Explainable and have it be specifically typed enough.
export function construct<T> (tgt: T, state: State<T>): T {
   return constructʹ(tgt, state) as T
}

// Dynamic version of construct.
function constructʹ (tgt: Value, state: Stateʹ): Value {
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
