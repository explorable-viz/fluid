import { absurd, className, error } from "./util/Core"
import { Cons, List, Nil, cons, nil } from "./BaseTypes2"
import { fieldVals } from "./DataType2"
import { Constr, State, Value, _, make } from "./Value2"

// Func to distinguish from expression-level Fun.
export abstract class Func<K> extends Value {
   abstract __apply (v: Value): [Env, K]
}

export class ConstrFunc<K> extends Func<K> {
   __apply (v: Value): [Env, K] {
      if (v instanceof Constr) {
         // Probably slow compared to visitor pattern :-o
         return (this as any as Func_State<K>)[className(v)].__apply(fieldVals(v))
      } else {
         return error("Not a datatype")
      }
   }
}

export abstract class ArgumentsFunc<K> extends Value {
   abstract __apply (v̅: Value[]): [Env, K]
}

// Can't add __apply to this because inconsistent with index signature.
export interface Func_State<K> extends State {
   [ctr: string]: ArgumentsFunc<K>
}

// Environments are snoc lists.
export abstract class Env implements Value {
   // Environment whose names have been projected away, leaving only list of values; cons rather than snoc, but doesn't matter.
   abstract entries (): List<Value>
   abstract get (k: string): Value | undefined

   has (k: string): boolean {
      return this.get(k) !== undefined
   }

   static singleton (k: string, v: Value): Env {
      return extendEnv(emptyEnv(), k, v)
   }

   static concat (ρ1: Env, ρ2: Env): Env {
      if (ρ2 instanceof EmptyEnv) {
         return ρ1
      } else
      if (ρ2 instanceof ExtendEnv) {
         return extendEnv(Env.concat(ρ1, ρ2.ρ), ρ2.k, ρ2.v)
      } else {
         return absurd()
      }
   }
}

export class EmptyEnv extends Env {
   entries (): Nil<Value> {
      return nil()
   }

   get (k: string): undefined {
      return undefined
   }
}

export function emptyEnv (): EmptyEnv {
   return make(EmptyEnv, {})
}

export class ExtendEnv extends Env {
   ρ: Env = _
   k: string = _
   v: Value = _

   entries (): Cons<Value> {
      return cons(this.v, this.ρ.entries())
   }

   get (k: string): Value | undefined {
      if (this.k === k) {
         return this.v
      } else {
         return this.ρ.get(k)
      }
   }
}

export function extendEnv (ρ: Env, k: string, v: Value): ExtendEnv {
   return make(ExtendEnv, { ρ, k, v })
}
