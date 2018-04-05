import { assert, make } from "./util/Core"
import { Id } from "./Runtime"
import { Expr } from "./Syntax"

export class EnvId extends Id {
   __EnvId (): void {
      // discriminator
   }

   static empty (): EmptyEnvId {
      return EmptyEnvId.make()
   }

   static singleton (k: EnvEntryId): EnvId {
      return ExtendEnvId.make(EmptyEnvId.make(), k)
   }

   static extend (j: EnvId, ks: EnvEntryId[]): EnvId {
      ks.forEach((k: EnvEntryId): void => {
         j = ExtendEnvId.make(j, k)
      })
      return j
   }

   static concat (j: EnvId, jʹ: EnvId): EnvId {
      if (jʹ instanceof EmptyEnvId) {
         return j
      } else
      if (jʹ instanceof ExtendEnvId) {
         return ExtendEnvId.make(EnvId.concat(j, jʹ.j), jʹ.k)
      } else {
         return assert(false)
      }
   }
}

export class EmptyEnvId extends EnvId { 
   static make (): EmptyEnvId {
      return make(EmptyEnvId)
   }
}

export class ExtendEnvId extends EnvId {
   j: EnvId
   k: EnvEntryId

   static make (j: EnvId, k: EnvEntryId): ExtendEnvId {
      const this_: ExtendEnvId = make(ExtendEnvId, j, k)
      this_.j = j
      this_.k = k
      return this_
   }
}

// Environments are snoc lists. An evaluation id is an expression id paired with the identity of all 
// environment entries used to close the term, in the order in which they were bound. This makes evaluation
// ids insensitive to the choice of names, and is essentially the same as the approach I used in my thesis. 
// But although evaluation ids do not depend on the ids of environments themselves, we must still intern
// environments in order to enforce the LVar semantics.

export abstract class Env {
   __Env(): void {
      // discriminator
   }

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

   get (k: string): EnvEntry | undefined {
      if (this.k === k) {
         return this.v
      } else {
         return this.ρ.get(k)
      }
   }
}

export class EnvEntryId {
   j: EnvId
   d: Expr.RecDefsId
   i: Expr.ExprId

   static make (j: EnvId, d: Expr.RecDefsId, i: Expr.ExprId): EnvEntryId {
      const this_: EnvEntryId = make(EnvEntryId, j, d, i)
      this_.j = j
      this_.d = d
      this_.i = i
      return this_
   }
}

export class EnvEntry {
   ρ: Env
   j: EnvId
   δ: Expr.RecDefs
   e: Expr.Expr

   static make (ρ: Env, j: EnvId, δ: Expr.RecDefs, e: Expr.Expr): EnvEntry {
      const this_: EnvEntry = make(EnvEntry)
      this_.ρ = ρ
      this_.j = j
      this_.δ = δ
      this_.e = e
      return this_
   }
}

export class RecDefs {
   __RecDefs (): void {
      // discriminator
   }
}

export class EmptyRecDefs extends RecDefs {
   static make (): EmptyRecDefs {
      return make(EmptyRecDefs)
   }
}

export class ExtendRecDefs extends RecDefs {
   δ: RecDefs
   def: Expr.RecDef

   static make (δ: RecDefs, def: Expr.RecDef): ExtendRecDefs {
      const this_: ExtendRecDefs = make(ExtendRecDefs, δ, def)
      this_.δ = δ
      this_.def = def
      return this_
   }
}
