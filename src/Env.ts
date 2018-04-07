import { assert, make } from "./util/Core"
import { Expr } from "./Syntax"

export class EnvEntries {
   __EnvEntries (): void {
      // discriminator
   }
}

export class EmptyEnvEntries extends EnvEntries { 
   static make (): EmptyEnvEntries {
      return make(EmptyEnvEntries)
   }
}

export class ExtendEnvEntries extends EnvEntries {
   j: EnvEntries
   entry: EnvEntry

   static make (j: EnvEntries, entry: EnvEntry): ExtendEnvEntries {
      const this_: ExtendEnvEntries = make(ExtendEnvEntries, j, entry)
      this_.j = j
      this_.entry = entry
      return this_
   }
}

// Environments are snoc lists. An evaluation id is an expression id paired with the identity of all 
// environment entries used to close the term, in the order in which they were bound. This makes evaluation
// ids insensitive to the choice of names, and is essentially the same as the approach I used in my thesis. 
// But although evaluation ids do not depend on the ids of environments themselves, we still intern
// environments to enable LVar semantics.

export abstract class Env {
   __Env(): void {
      // discriminator
   }

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
         return assert(false)
      }
   }
}

export class EmptyEnv extends Env {
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

   static make (ρ: Env, k: string, v: EnvEntry): ExtendEnv {
      const this_: ExtendEnv = make(ExtendEnv, ρ, k, v)
      this_.ρ = ρ
      this_.k = k
      this_.v = v
      return this_
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

export class EnvEntry {
   ρ: Env
   δ: Expr.RecDefs
   e: Expr.Expr

   static make (ρ: Env, δ: Expr.RecDefs, e: Expr.Expr): EnvEntry {
      const this_: EnvEntry = make(EnvEntry, ρ, δ, e)
      this_.ρ = ρ
      this_.δ = δ
      this_.e = e
      return this_
   }
}
