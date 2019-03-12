import { absurd } from "./util/Core"
import { Persistent, PersistentObject, make } from "./util/Persistent"
import { Cons, List, Nil } from "./BaseTypes"
import { Value } from "./ExplVal"

// Environments are snoc lists. An evaluation id is an expression id paired with the identity of all 
// environment entries used to close the term, in the order in which they were bound. This makes evaluation
// ids insensitive to the choice of names, and is similar to the approach I used in my thesis. 
// But although evaluation ids do not depend on the ids of environments themselves, we still intern
// environments to enable LVar semantics.

export abstract class Env implements PersistentObject {
   // Environment whose names have been projected away, leaving only list of values; cons rather than snoc, but doesn't matter.
   abstract entries (): List<Value>
   abstract get (k: string): Value | undefined
   abstract constructor_ (...args: Persistent[]): void

   has (k: string): boolean {
      return this.get(k) !== undefined
   }

   static empty (): EmptyEnv {
      return EmptyEnv.make()
   }

   static singleton (k: string, v: Value): Env {
      return ExtendEnv.make(Env.empty(), k, v)
   }

   static extend (ρ: Env, kvs: [string, Value][]): Env {
      kvs.forEach(([k, v]: [string, Value]) => {
         ρ = ExtendEnv.make(ρ, k, v)
      })
      return ρ
   }

   static concat (ρ1: Env, ρ2: Env): Env {
      if (ρ2 instanceof EmptyEnv) {
         return ρ1
      } else
      if (ρ2 instanceof ExtendEnv) {
         return ExtendEnv.make(Env.concat(ρ1, ρ2.ρ), ρ2.k, ρ2.v)
      } else {
         return absurd()
      }
   }
}

export class EmptyEnv extends Env {
   constructor_ () {
   }

   static make (): EmptyEnv {
      return make(EmptyEnv)
   }

   entries (): Nil<Value> {
      return Nil.make()
   }

   get (k: string): undefined {
      return undefined
   }
}

export class ExtendEnv extends Env {
   ρ: Env
   k: string
   v: Value

   constructor_ (ρ: Env, k: string, v: Value) {
      this.ρ = ρ
      this.k = k
      this.v = v
   }

   static make (ρ: Env, k: string, v: Value): ExtendEnv {
      return make(ExtendEnv, ρ, k, v)
   }

   entries (): Cons<Value> {
      return Cons.make(this.v, this.ρ.entries())
   }

   get (k: string): Value | undefined {
      if (this.k === k) {
         return this.v
      } else {
         return this.ρ.get(k)
      }
   }
}
