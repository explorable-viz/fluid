import { assert, make } from "./util/Core"
import { Id } from "./Memo"
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
   static make (): EnvId {
      return make(EnvId)
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

// Environments are snoc lists. The identity of an evaluated term is the identity of the original expression
// paired with the identity of all environment entries used to close the term, in the order in which they 
// were bound. This makes identity insensitive to the choice of names, and is essentially the same as the
// approach I used in my thesis ("translating" every function body by the identity of the argument used to
// close it). This is *not* the same as hash-consing environments (which would consider the keys as well).
// Prefer inductive definition to an array, to align with definition of environment "ids".

export abstract class Env {
   __Env(): void {
      // discriminator
   }

   abstract get (k: string): EnvEntry | undefined;

   has (k: string): boolean {
      return this.get(k) !== undefined
   }

   static empty (): EmptyEnv {
      return new EmptyEnv
   }

   static singleton (k: string, v: EnvEntry): Env {
      return new ExtendEnv(new EmptyEnv, k, v)
   }

   static extend (ρ: Env, kvs: [string, EnvEntry][]): Env {
      kvs.forEach(([k, v]: [string, EnvEntry]) => {
         ρ = new ExtendEnv(ρ, k, v)
      })
      return ρ
   }

   static concat (ρ1: Env, ρ2: Env): Env {
      if (ρ2 instanceof EmptyEnv) {
         return ρ1
      } else
      if (ρ2 instanceof ExtendEnv) {
         return new ExtendEnv(Env.concat(ρ1, ρ2.ρ), ρ2.k, ρ2.v)
      } else {
         return assert(false)
      }
   }
}

export class EmptyEnv extends Env {
   get (k: string): undefined {
      return undefined
   }
}

export class ExtendEnv extends Env {
   ρ: Env
   k: string
   v: EnvEntry

   constructor (ρ: Env, k: string, v: EnvEntry) {
      super()
      this.ρ = ρ
      this.k = k
      this.v = v
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
   i: Expr.RecDefinitionId
   iʹ: Expr.ExprId

   static make (j: EnvId, i: Expr.RecDefinitionId, iʹ: Expr.ExprId): EnvEntryId {
      const this_: EnvEntryId = make(EnvEntryId, j, i, iʹ)
      this_.j = j
      this_.i = i
      this_.iʹ = iʹ
      return this_
   }
}

export class EnvEntry {
   ρ: Env
   j: EnvId,
   δ: Expr.RecDefinition[]
   e: Expr.Expr

   constructor(ρ: Env, j: EnvId, δ: Expr.RecDefinition[], e: Expr.Expr) {
      this.ρ = ρ
      this.j = j
      this.δ = δ
      this.e = e
   }
}
