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
      __Value_Value(): void {
         // discriminator
      }
   }

   export class Closure extends Value {
      ρ: Env
      σ: Traced.Trie<Traced>
   
      static at (α: PersistentObject, ρ: Env, σ: Traced.Trie<Traced>): Closure {
         const this_: Closure = create(α, Closure)
         this_.ρ = ρ
         this_.σ = σ
         this_.__version()
         return this_
      }
   }

   export class Prim extends Value {
      __Value_Prim(): void {
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

// Called ExplVal in the formalism. Interned rather than versioned.
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

export type Trace = Traced.Trace

export namespace Traced {
   export type Args<K> = Args.Args<K>

   export namespace Args {
      // n-ary product
      export class Args<K> extends PersistentObject {
         __Traced_Args (κ: K): void {
            // discriminator
         }
      }

      // Maps zero arguments to κ.
      export class End<K> extends Args<K> {
         κ: K

         static is<K> (Π: Args<K>): Π is End<K> {
            return Π instanceof End
         }

         static make<K> (κ: K): End<K> {
            const this_: End<K> = make<End<K>>(End, κ)
            this_.κ = κ
            return this_
         }
      }

      // Maps a single argument to another args trie.
      export class Next<K> extends Args<K> {
         σ: Trie<Args<K>>

         static is<K> (Π: Args<K>): Π is Next<K> {
            return Π instanceof Next
         }

         static make<K> (σ: Trie<Args<K>>): Next<K> {
            const this_: Next<K> = make<Next<K>>(Next, σ)
            this_.σ = σ
            return this_
         }
      }

      export class Top<K> extends Args<K> {
         κ: K // want fix at null but couldn't make that work with the polymorphism

         static is<K> (Π: Args<K>): Π is Top<K> {
            return Π instanceof Top
         }

         static make<K> (κ: K): Top<K> {
            const this_: Top<K> = make<Top<K>>(Top, κ)
            this_.κ = κ
            return this_
         }
      }
   }

   // Tries are persistent but not versioned, as per the formalism.
   export type Trie<K> = Trie.Trie<K>

   export type Kont = Traced | Args<any> | Trie<any>

   export namespace Trie {
      export abstract class Trie<K> extends PersistentObject {
         __Trie_Trie (κ: K): void {
            // discriminator
         }
      }

      export class Prim<K> extends Trie<K> {
         κ: K
      }

      export class ConstInt<K> extends Prim<K> {
         static is<K> (σ: Trie<K>): σ is ConstInt<K> {
            return σ instanceof ConstInt
         }

         static make<K> (κ: K): ConstInt<K> {
            const this_: ConstInt<K> = make<ConstInt<K>>(ConstInt, κ)
            this_.κ = κ
            return this_
         }
      }

      export class ConstStr<K> extends Prim<K> {
         static is<K> (σ: Trie<K>): σ is ConstStr<K> {
            return σ instanceof ConstStr
         }

         static make<K> (κ: K): ConstStr<K> {
            const this_: ConstStr<K> = make<ConstStr<K>>(ConstStr, κ)
            this_.κ = κ
            return this_
         }
      }

      export class Constr<K> extends Trie<K> {
         cases: FiniteMap<string, Args<K>>

         static is<K> (σ: Trie<K>): σ is Constr<K> {
            return σ instanceof Constr
         }

         static make<K> (cases: FiniteMap<string, Args<K>>): Constr<K> {
            const this_: Constr<K> = make<Constr<K>>(Constr, cases)
            this_.cases = cases
            return this_
         }
      }

      export class Fun<K> extends Trie<K> {
         κ: K

         static is<K> (σ: Trie<K>): σ is Fun<K> {
            return σ instanceof Fun
         }

         static make<K> (κ: K): Fun<K> {
            const this_: Fun<K> = make<Fun<K>>(Fun, κ)
            this_.κ = κ
            return this_
         }
      }

      export class Var<K> extends Trie<K> {
         x: Lex.Var
         κ: K

         static is<K> (σ: Trie<K>): σ is Var<K> {
            return σ instanceof Var
         }

         static make<K> (x: Lex.Var, κ: K): Var<K> {
            const this_: Var<K> = make<Var<K>>(Var, x, κ)
            this_.x = x
            this_.κ = κ
            return this_
         }
      }

      // Wanted to fix K at null but that doesn't work with polymorphic code.
      export class Top<K> extends Trie<K> {
         κ: K

         static is<K> (σ: Trie<K>): σ is Top<K> {
            return σ instanceof Top
         }

         static make<K> (κ: K): Top<K> {
            const this_: Top<K> = make<Top<K>>(Top, κ)
            this_.κ = κ
            return this_
         }
      }
   }

   export class TracedMatch<K> extends PersistentObject {
      t: Trace | null // null iff ξ represents a dead branch
      ξ: Match<K>

      static make<K> (t: Trace | null, ξ: Match<K>): TracedMatch<K> {
         const this_: TracedMatch<K> = make<TracedMatch<K>>(TracedMatch, t, ξ)
         this_.t = t
         this_.ξ = ξ
         return this_
      }
   }

   export type Match<K> = Match.Match<K>

   // A trie which has been matched (executed) to a depth of at least one.
   export namespace Match {
      export type Args<K> = Args.Args<K>

      export namespace Args {
         export class Args<K> extends PersistentObject {
            __Match_Args (): void {
               // discriminator
            }
         }
   
         export class End<K> extends Args<K> {
            κ: K
   
            static is<K> (Ψ: Args<K>): Ψ is End<K> {
               return Ψ instanceof End
            }
   
            static make<K> (κ: K): End<K> {
               const this_: End<K> = make<End<K>>(End, κ)
               this_.κ = κ
               return this_
            }
         }
   
         export class Next<K> extends Args<K> {
            tξ: TracedMatch<K>
   
            static is<K> (Ψ: Args<K>): Ψ is Next<K> {
               return Ψ instanceof Next
            }
   
            static make<K> (tξ: TracedMatch<K>): Next<K> {
               const this_: Next<K> = make<Next<K>>(Next, tξ)
               this_.tξ = tξ
               return this_
            }
         }
      }

      export class Match<K> extends PersistentObject {
         __Match_Match (): void {
            // discriminator
         }
      }

      export class Prim<K> extends Match<K> {
         κ: K
      }

      export class ConstInt<K> extends Prim<K> {
         val: number

         static is<K> (ξ: Match<K>): ξ is ConstInt<K> {
            return ξ instanceof ConstInt
         }

         static make<K> (val: number, κ: K): ConstInt<K> {
            const this_: ConstInt<K> = make<ConstInt<K>>(ConstInt, val, κ)
            this_.val = val
            this_.κ = κ
            return this_
         }
      }

      export class ConstStr<K> extends Prim<K> {
         val: string

         static is<K> (ξ: Match<K>): ξ is ConstStr<K> {
            return ξ instanceof ConstStr
         }

         static make<K> (val: string, κ: K): ConstStr<K> {
            const this_: ConstStr<K> = make<ConstStr<K>>(ConstStr, val, κ)
            this_.val = val
            this_.κ = κ
            return this_
         }
      }

      // Exactly one branch will be live (i.e. an instanceof Match.Args rather than Trie.Args).
      export class Constr<K> extends Match<K> {
         cases: FiniteMap<string, Traced.Args<K> | Args<K>> 

         static is<K> (ξ: Match<K>): ξ is Constr<K> {
            return ξ instanceof Constr
         }

         static make<K> (cases: FiniteMap<string, Traced.Args<K> | Args<K>>): Constr<K> {
            const this_: Constr<K> = make(Constr, cases)
            this_.cases = cases
            return this_
         }
      }

      export class Fun<K> extends Match<K> {
         f: Value.Closure | Value.PrimOp
         κ: K

         static is<K> (ξ: Match<K>): ξ is Fun<K> {
            return ξ instanceof Fun
         }

         static make<K> (f: Value.Closure | Value.PrimOp, κ: K): Fun<K> {
            const this_: Fun<K> = make<Fun<K>>(Fun, f, κ)
            this_.f = f
            this_.κ = κ
            return this_
         }
      }

      export class Var<K> extends Match<K> {
         x: Lex.Var
         v: Value | null
         κ: K

         static is<K> (ξ: Match<K>): ξ is Var<K> {
            return ξ instanceof Var
         }

         static make<K> (x: Lex.Var, v: Value | null, κ: K): Var<K> {
            const this_: Var<K> = make<Var<K>>(Var, x, v, κ)
            this_.x = x
            this_.v = v
            this_.κ = κ
            return this_
         }
      }
   }

   export class Trace extends VersionedObject<Runtime<Expr>> {
      __Trace_Trace(): void {
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
      σ: Trie.Var<Traced>
      t: Trace | null

      __Trace_Let (): void {
         // discriminator
      }

      static at (k: Runtime<Expr>, tu: Traced, σ: Trie.Var<Traced>, t: Trace | null): Let {
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
      σ: Trie<Traced>
      t: Trace | null

      __Trace_MatchAs (): void {
         // discriminator
      }

      static at (k: Runtime<Expr>, tu: Traced, σ: Trie<Traced>,  t: Trace | null): MatchAs {
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
