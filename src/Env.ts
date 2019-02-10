import { absurd } from "./util/Core"
import { PersistentObject, make } from "./util/Persistent"
import { List } from "./BaseTypes"
import { Traced } from "./Traced"

import RecDef = Traced.RecDef

export abstract class EnvEntries extends PersistentObject {
   __subtag: "EnvEntries"
}

export class EmptyEnvEntries extends EnvEntries { 
   constructor_ () {
   }

   static make (): EmptyEnvEntries {
      return make(EmptyEnvEntries)
   }
}

export class ExtendEnvEntries extends EnvEntries {
   j: EnvEntries
   entry: EnvEntry

   constructor_ (
      j: EnvEntries,
      entry: EnvEntry
   ) {
      this.j = j
      this.entry = entry
   }

   static make (j: EnvEntries, entry: EnvEntry): ExtendEnvEntries {
      return make(ExtendEnvEntries, j, entry)
   }
}

// Environments are snoc lists. An evaluation id is an expression id paired with the identity of all 
// environment entries used to close the term, in the order in which they were bound. This makes evaluation
// ids insensitive to the choice of names, and is similar to the approach I used in my thesis. 
// But although evaluation ids do not depend on the ids of environments themselves, we still intern
// environments to enable LVar semantics.

export abstract class Env extends PersistentObject {
   __subtag: "Env"

   abstract entries (): EnvEntries;

   abstract get (k: string): EnvEntry | undefined;

   has (k: string): boolean {
      return this.get(k) !== undefined
   }

   static empty (): EmptyEnv {
      return EmptyEnv.make()
   }

   static singleton (k: string, v: EnvEntry): Env {
      return ExtendEnv.make(Env.empty(), k, v)
   }

   static extend (ρ: Env, kvs: [string, EnvEntry][]): Env {
      kvs.forEach(([k, v]: [string, EnvEntry]) => {
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

   entries (): EmptyEnvEntries {
      return EmptyEnvEntries.make()
   }

   get (k: string): undefined {
      return undefined
   }
}

export class ExtendEnv extends Env {
   ρ: Env
   k: string
   v: EnvEntry

   constructor_ (
      ρ: Env,
      k: string,
      v: EnvEntry
   ) {
      this.ρ = ρ
      this.k = k
      this.v = v
   }

   static make (ρ: Env, k: string, v: EnvEntry): ExtendEnv {
      return make(ExtendEnv, ρ, k, v)
   }

   entries (): ExtendEnvEntries {
      return ExtendEnvEntries.make(this.ρ.entries(), this.v)
   }

   get (k: string): EnvEntry | undefined {
      if (this.k === k) {
         return this.v
      } else {
         return this.ρ.get(k)
      }
   }
}

export class EnvEntry extends PersistentObject {
   ρ: Env
   δ: List<RecDef>
   e: Traced

   constructor_ (
      ρ: Env,
      δ: List<RecDef>,
      e: Traced
   ) {
      this.ρ = ρ
      this.δ = δ
      this.e = e
   }

   static make (ρ: Env, δ: List<RecDef>, e: Traced): EnvEntry {
      return make(EnvEntry, ρ, δ, e)
   }
}
