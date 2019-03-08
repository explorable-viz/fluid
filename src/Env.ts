import { absurd } from "./util/Core"
import { Persistent, PersistentObject, make } from "./util/Persistent"
import { Cons, List, Nil } from "./BaseTypes"
import { Traced } from "./Traced"

// Environments are snoc lists. An evaluation id is an expression id paired with the identity of all 
// environment entries used to close the term, in the order in which they were bound. This makes evaluation
// ids insensitive to the choice of names, and is similar to the approach I used in my thesis. 
// But although evaluation ids do not depend on the ids of environments themselves, we still intern
// environments to enable LVar semantics.

export abstract class Env implements PersistentObject {
   // Environment whose names have been projected away, leaving only list of values; cons rather than snoc, but doesn't matter.
   abstract entries (): List<Traced>
   abstract get (k: string): Traced | undefined
   abstract constructor_ (...args: Persistent[]): void

   has (k: string): boolean {
      return this.get(k) !== undefined
   }

   static empty (): EmptyEnv {
      return EmptyEnv.make()
   }

   static singleton (k: string, tv: Traced): Env {
      return ExtendEnv.make(Env.empty(), k, tv)
   }

   static extend (ρ: Env, kvs: [string, Traced][]): Env {
      kvs.forEach(([k, tv]: [string, Traced]) => {
         ρ = ExtendEnv.make(ρ, k, tv)
      })
      return ρ
   }

   static concat (ρ1: Env, ρ2: Env): Env {
      if (ρ2 instanceof EmptyEnv) {
         return ρ1
      } else
      if (ρ2 instanceof ExtendEnv) {
         return ExtendEnv.make(Env.concat(ρ1, ρ2.ρ), ρ2.k, ρ2.tv)
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

   entries (): Nil<Traced> {
      return Nil.make()
   }

   get (k: string): undefined {
      return undefined
   }
}

export class ExtendEnv extends Env {
   ρ: Env
   k: string
   tv: Traced

   constructor_ (
      ρ: Env,
      k: string,
      tv: Traced
   ) {
      this.ρ = ρ
      this.k = k
      this.tv = tv
   }

   static make (ρ: Env, k: string, tv: Traced): ExtendEnv {
      return make(ExtendEnv, ρ, k, tv)
   }

   entries (): Cons<Traced> {
      return Cons.make(this.tv, this.ρ.entries())
   }

   get (k: string): Traced | undefined {
      if (this.k === k) {
         return this.tv
      } else {
         return this.ρ.get(k)
      }
   }
}
