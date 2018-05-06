import { make } from "./util/Core"
import { List } from "./BaseTypes"
import { Env } from "./Env"
import { FiniteMap } from "./FiniteMap"
import { Runtime } from "./Eval"
import { Expr, Lex } from "./Expr"
import { UnaryOp } from "./Primitive"
import { VersionedObject, PersistentObject, create } from "./Runtime"

export type Value = Value.Value

export namespace Value {
   export class Value extends VersionedObject {
      __Value(): void {
         // discriminator
      }
   }

   export class Closure extends Value {
      ρ: Env
      σ: Trie
   
      static at (α: PersistentObject, ρ: Env, σ: Trie): Closure {
         const this_: Closure = create(α, Closure)
         this_.ρ = ρ
         this_.σ = σ
         this_.__version()
         return this_
      }
   }

   export class Prim extends Value {
      __Prim(): void {
         // discriminator
      }
   }
   
   export class ConstInt extends Prim {
      val: number
   
      static at (α: PersistentObject, val: number): ConstInt {
         const this_: ConstInt = create(α, ConstInt)
         this_.val = val
         this_.__version()
         return this_
      }

      toString (): string {
         return `${this.val}`
      }
   }
   
   export class ConstStr extends Prim {
      val: string
   
      static at (α: PersistentObject, val: string): ConstStr {
         const this_: ConstStr = create(α, ConstStr)
         this_.val = val
         this_.__version()
         return this_
      }

      toString (): string {
         return `"${this.val}"`
      }
   }
   
   export class Constr extends Value {
      ctr: Lex.Ctr
      args: List<Traced>
   
      static at (α: PersistentObject, ctr: Lex.Ctr, args: List<Traced>): Constr {
         const this_: Constr = create(α, Constr)
         this_.ctr = ctr
         this_.args = args
         this_.__version()
         return this_
      }
   }

   export class PrimOp extends Value {
      op: UnaryOp
   
      static at (α: PersistentObject, op: UnaryOp): PrimOp {
         const this_: PrimOp = create(α, PrimOp)
         this_.op = op
         this_.__version()
         return this_
      }
   }
}

// Rename to Explained?
export class Traced extends PersistentObject {
   t: Trace
   v: Value | null

   static make (t: Trace, v: Value | null): Traced {
      const this_: Traced = make(Traced, t, v)
      this_.t = t
      this_.v = v
      return this_
   }
}

// Tries used to have type parameter K, as per the formalism, but in TypeScript it didn't really help.
export type Kont = Traced | Trie | null

// Tries are persistent but not versioned, as per the formalism.
export type Trie = Trie.Trie

export namespace Trie {
   export class Trie extends PersistentObject {
      __Trie (): void {
         // discriminator
      }
   }

   export class Prim extends Trie {
      κ: Kont
   }

   export class ConstInt extends Prim {
      static make (κ: Kont): ConstInt {
         const this_: ConstInt = make(ConstInt, κ)
         this_.κ = κ
         return this_
      }
   }

   export class ConstStr extends Prim {
      static make (κ: Kont): ConstStr {
         const this_: ConstStr = make(ConstStr, κ)
         this_.κ = κ
         return this_
      }
   }

   export class Constr extends Trie {
      cases: FiniteMap<string, Kont>

      static make (cases: FiniteMap<string, Kont>): Constr {
         const this_: Constr = make(Constr, cases)
         this_.cases = cases
         return this_
      }
   }

   export class Fun extends Trie {
      κ: Kont

      static make (κ: Kont): Fun {
         const this_: Fun = make(Fun, κ)
         this_.κ = κ
         return this_
      }
   }

   export class Var extends Trie {
      x: Lex.Var
      κ: Kont

      static make (x: Lex.Var, κ: Kont): Var {
         const this_: Var = make(Var, x, κ)
         this_.x = x
         this_.κ = κ
         return this_
      }
   }
}

export class TracedMatchedTrie extends PersistentObject {
   t: Trace
   ξ: MatchedTrie

   static make (t: Trace, ξ: MatchedTrie): TracedMatchedTrie {
      const this_: TracedMatchedTrie = make(TracedMatchedTrie, t, ξ)
      this_.t = t
      this_.ξ = ξ
      return this_
   }
}

// Matched tries should have (executed) traced values as their bodies, but that requires more plugging in.
export type MatchedKont = Traced | Trie | TracedMatchedTrie | null

export type MatchedTrie = MatchedTrie.MatchedTrie

export namespace MatchedTrie {
   export class MatchedTrie extends PersistentObject {
   }

   export class Prim extends MatchedTrie {
      κ: Kont
   }

   export class ConstInt extends Prim {
      val: number

      static make (val: number, κ: Kont): ConstInt {
         const this_: ConstInt = make(ConstInt, val, κ)
         this_.val = val
         this_.κ = κ
         return this_
      }
   }

   export class ConstStr extends Prim {
      val: string

      static make (val: string, κ: Kont): ConstStr {
         const this_: ConstStr = make(ConstStr, val, κ)
         this_.val = val
         this_.κ = κ
         return this_
      }
   }

   export class Constr extends MatchedTrie {
      cases: FiniteMap<string, MatchedKont> // exactly one will be "live"

      static make (cases: FiniteMap<string, MatchedKont>): Constr {
         const this_: Constr = make(Constr, cases)
         this_.cases = cases
         return this_
      }
   }

   export class Fun extends MatchedTrie {
      ρ: Env
      σ: Trie
      κ: Kont

      static make (ρ: Env, σ: Trie, κ: Kont): Fun {
         const this_: Fun = make(Fun, ρ, σ, κ)
         this_.ρ = ρ
         this_.σ = σ
         this_.κ = κ
         return this_
      }
   }

   // Any extra information a matched variable trie should carry?
   export class Var extends MatchedTrie {
      x: Lex.Var
      κ: Kont

      static make (x: Lex.Var, κ: Kont): Var {
         const this_: Var = make(Var, x, κ)
         this_.x = x
         this_.κ = κ
         return this_
      }
   }
}

export type Trace = Trace.Trace

export namespace Trace {
   export class Trace extends VersionedObject<Runtime<Expr>> {
      __Trace(): void {
         // discriminator
      }
   }
   
   export class App extends Trace {
      func: Traced
      arg: Traced
      body: Trace | null

      static at (k: Runtime<Expr>, func: Traced, arg: Traced, body: Trace | null): App {
         const this_: App = create(k, App)
         this_.func = func
         this_.arg = arg
         this_.body = body
         this_.__version()
         return this_
      }
   }

   // Not the same as ⊥ (null); we distinguish information about an absence from the absence of information.
   export class Empty extends Trace {
      static at (k: Runtime<Expr>): Empty {
         const this_: Empty = create(k, Empty)
         this_.__version()
         return this_
      }
   }

   export class Let extends Trace {
      tu: Traced
      σ: Trie.Var
      t: Trace | null

      __Let (): void {
         // discriminator
      }

      static at (k: Runtime<Expr>, tu: Traced, σ: Trie.Var, t: Trace | null): Let {
         const this_: Let = create(k, Let)
         this_.tu = tu
         this_.σ = σ
         this_.t = t
         this_.__version()
         return this_
      }
   }

   export class RecDef extends VersionedObject<Runtime<Expr.RecDef>> {
      x: Lex.Var
      tv: Traced
   
      static at (i: Runtime<Expr.RecDef>, x: Lex.Var, tv: Traced): RecDef {
         const this_: RecDef = create(i, RecDef)
         this_.x = x
         this_.tv = tv
         this_.__version()
         return this_
      }
   }

   // Continuation here should really be a trace, not a traced value.
   export class LetRec extends Trace {
      δ: List<RecDef>
      tv: Traced
   
      static at (k: Runtime<Expr>, δ: List<RecDef>, tv: Traced): LetRec {
         const this_: LetRec = create(k, LetRec)
         this_.δ = δ
         this_.tv = tv
         this_.__version()
         return this_
      }
   }
   
   export class MatchAs extends Trace {
      tu: Traced
      σ: Trie
      t: Trace | null

      __Match (): void {
         // discriminator
      }

      static at (k: Runtime<Expr>, tu: Traced, σ: Trie,  t: Trace | null): MatchAs {
         const this_: MatchAs = create(k, MatchAs)
         this_.tu = tu
         this_.σ = σ
         this_.t = t
         this_.__version()
         return this_
      }
   }

   export class PrimApp extends Trace {
      tv1: Traced
      opName: Lex.OpName
      tv2: Traced

      static at (k: Runtime<Expr>, tv1: Traced, opName: Lex.OpName, tv2: Traced): PrimApp {
         const this_: PrimApp = create(k, PrimApp)
         this_.tv1 = tv1
         this_.opName = opName
         this_.tv2 = tv2
         this_.__version()
         return this_
      }
   }

   export class Var extends Trace {
      x: Lex.Var
      t: Trace | null

      static at (k: Runtime<Expr>, x: Lex.Var, t: Trace | null): Var {
         const this_: Var = create(k, Var)
         this_.x = x
         this_.t = t
         this_.__version()
         return this_
      }
   }
}
