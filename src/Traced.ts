import { make, PersistentObject } from "./util/Core"
import { List } from "./BaseTypes"
import { Env } from "./Env"
import { FiniteMap } from "./FiniteMap"
import { Runtime } from "./Eval"
import { Expr, Lex } from "./Expr"
import { UnaryOp } from "./Primitive"
import { InternedObject, VersionedObject, at } from "./Runtime"

export type Value = Value.Value

export namespace Value {
   export class Value extends VersionedObject {
      __Value_Value (): void {
         // discriminator
      }
   }

   export class Closure extends Value {
      ρ: Env
      σ: Traced.Trie<Traced>
   
      constructor_ (ρ: Env, σ: Traced.Trie<Traced>): void {
         this.ρ = ρ
         this.σ = σ
      }

      static at (α: PersistentObject, ρ: Env, σ: Traced.Trie<Traced>): Closure {
         return at(α, Closure, ρ, σ)
      }
   }

   export class Prim extends Value {
      __Value_Prim (): void {
         // discriminator
      }
   }
   
   export class ConstInt extends Prim {
      val: number

      constructor_ (val: number): void {
         this.val = val
      }
   
      static at (α: PersistentObject, val: number): ConstInt {
         return at(α, ConstInt, val)
      }

      toString (): string {
         return `${this.val}`
      }
   }
   
   export class ConstStr extends Prim {
      val: string

      constructor_ (val: string): void {
         this.val = val
      }
   
      static at (α: PersistentObject, val: string): ConstStr {
         return at(α, ConstStr, val)
      }

      toString (): string {
         return `"${this.val}"`
      }
   }
   
   export class Constr extends Value {
      ctr: Lex.Ctr
      args: List<Traced>

      constructor_ (ctr: Lex.Ctr, args: List<Traced>): void {
         this.ctr = ctr
         this.args = args
      }
   
      static at (α: PersistentObject, ctr: Lex.Ctr, args: List<Traced>): Constr {
         return at(α, Constr, ctr, args)
      }
   }

   export class PrimOp extends Value {
      op: UnaryOp

      constructor_ (op: UnaryOp): void {
         this.op = op
      }
   
      static at (α: PersistentObject, op: UnaryOp): PrimOp {
         return at(α, PrimOp, op)
      }
   }
}

// Called ExplVal in the formalism.
export class Traced extends InternedObject {
   constructor (
      public t: Trace,
      public v: Value | null
   ) {
      super()
   }

   static make (t: Trace, v: Value | null): Traced {
      return make(Traced, t, v)
   }
}

export type Trace = Traced.Trace

export namespace Traced {
   export type Args<K> = Args.Args<K>

   export namespace Args {
      // n-ary product
      export class Args<K> extends InternedObject {
         __Traced_Args (κ: K): void {
            // discriminator
         }
      }

      // Maps zero arguments to κ.
      export class End<K> extends Args<K> {
         constructor (
            public κ: K
         ) {
            super()
         }

         static is<K> (Π: Args<K>): Π is End<K> {
            return Π instanceof End
         }

         static make<K> (κ: K): End<K> {
            return make<End<K>>(End, κ)
         }
      }

      // Maps a single argument to another args trie.
      export class Next<K> extends Args<K> {
         constructor (
            public σ: Trie<Args<K>>
         ) {
            super()
         }

         static is<K> (Π: Args<K>): Π is Next<K> {
            return Π instanceof Next
         }

         static make<K> (σ: Trie<Args<K>>): Next<K> {
            return make<Next<K>>(Next, σ)
         }
      }

      export class Top<K> extends Args<K> {
         constructor (
            public κ: K // want fix at null but couldn't make that work with the polymorphism
         ) {
            super()
         }

         static is<K> (Π: Args<K>): Π is Top<K> {
            return Π instanceof Top
         }

         static make<K> (κ: K): Top<K> {
            return make<Top<K>>(Top, κ)
         }
      }
   }

   // Tries are interned rather than versioned, as per the formalism.
   export type Trie<K> = Trie.Trie<K>

   export type Kont = Traced | Args<any> | Trie<any>

   export namespace Trie {
      export abstract class Trie<K> extends InternedObject {
         __Trie_Trie (κ: K): void {
            // discriminator
         }
      }

      export class Prim<K> extends Trie<K> {
         constructor (
            public κ: K
         ) {
            super()
         }
      }

      export class ConstInt<K> extends Prim<K> {
         static is<K> (σ: Trie<K>): σ is ConstInt<K> {
            return σ instanceof ConstInt
         }

         static make<K> (κ: K): ConstInt<K> {
            return make<ConstInt<K>>(ConstInt, κ)
         }
      }

      export class ConstStr<K> extends Prim<K> {
         static is<K> (σ: Trie<K>): σ is ConstStr<K> {
            return σ instanceof ConstStr
         }

         static make<K> (κ: K): ConstStr<K> {
            return make<ConstStr<K>>(ConstStr, κ)
         }
      }

      export class Constr<K> extends Trie<K> {
         constructor (
            public cases: FiniteMap<string, Args<K>>
         ) {
            super()
         }

         static is<K> (σ: Trie<K>): σ is Constr<K> {
            return σ instanceof Constr
         }

         static make<K> (cases: FiniteMap<string, Args<K>>): Constr<K> {
            return make<Constr<K>>(Constr, cases)
         }
      }

      export class Fun<K> extends Trie<K> {
         constructor (
            public κ: K
         ) {
            super()
         }

         static is<K> (σ: Trie<K>): σ is Fun<K> {
            return σ instanceof Fun
         }

         static make<K> (κ: K): Fun<K> {
            return make<Fun<K>>(Fun, κ)
         }
      }

      export class Var<K> extends Trie<K> {
         constructor (
            public x: Lex.Var,
            public κ: K
         ) {
            super()
         }

         static is<K> (σ: Trie<K>): σ is Var<K> {
            return σ instanceof Var
         }

         static make<K> (x: Lex.Var, κ: K): Var<K> {
            return make<Var<K>>(Var, x, κ)
         }
      }

      // Wanted to fix K at null but that doesn't work with polymorphic code.
      export class Top<K> extends Trie<K> {
         constructor (
            public κ: K
         ) {
            super()
         }

         static is<K> (σ: Trie<K>): σ is Top<K> {
            return σ instanceof Top
         }

         static make<K> (κ: K): Top<K> {
            return make<Top<K>>(Top, κ)
         }
      }
   }

   export class TracedMatch<K> extends InternedObject {
      constructor (
         public t: Trace | null, // null iff ξ represents a dead branch
         public ξ: Match<K>
      ) {
         super()
      }

      static make<K> (t: Trace | null, ξ: Match<K>): TracedMatch<K> {
         return make<TracedMatch<K>>(TracedMatch, t, ξ)
      }
   }

   export type Match<K> = Match.Match<K>

   // A trie which has been matched (executed) to a depth of at least one.
   export namespace Match {
      export type Args<K> = Args.Args<K>

      export namespace Args {
         export class Args<K> extends InternedObject {
            __Match_Args (): void {
               // discriminator
            }
         }
   
         export class End<K> extends Args<K> {
            constructor (
               public κ: K
            ) {
               super()
            }
   
            static is<K> (Ψ: Args<K>): Ψ is End<K> {
               return Ψ instanceof End
            }
   
            static make<K> (κ: K): End<K> {
               return make<End<K>>(End, κ)
            }
         }
   
         export class Next<K> extends Args<K> {
            constructor (
               public tξ: TracedMatch<K>
            ) {
               super()
            }
   
            static is<K> (Ψ: Args<K>): Ψ is Next<K> {
               return Ψ instanceof Next
            }
   
            static make<K> (tξ: TracedMatch<K>): Next<K> {
               return make<Next<K>>(Next, tξ)
            }
         }
      }

      export class Match<K> extends InternedObject {
         __Match_Match (): void {
            // discriminator
         }
      }

      export class Prim<K> extends Match<K> {
         constructor (
            public κ: K
         ) {
            super()
         }
      }

      export class ConstInt<K> extends Prim<K> {
         constructor (
            public val: number,
            κ: K
         ) {
            super(κ)
         }

         static is<K> (ξ: Match<K>): ξ is ConstInt<K> {
            return ξ instanceof ConstInt
         }

         static make<K> (val: number, κ: K): ConstInt<K> {
            return make<ConstInt<K>>(ConstInt, val, κ)
         }
      }

      export class ConstStr<K> extends Prim<K> {
         constructor (
            public val: string,
            κ: K
         ) {
            super(κ)
         }

         static is<K> (ξ: Match<K>): ξ is ConstStr<K> {
            return ξ instanceof ConstStr
         }

         static make<K> (val: string, κ: K): ConstStr<K> {
            return make<ConstStr<K>>(ConstStr, val, κ)
         }
      }

      // Exactly one branch will be live (i.e. an instanceof Match.Args rather than Trie.Args).
      export class Constr<K> extends Match<K> {
         constructor (
            public cases: FiniteMap<string, Traced.Args<K> | Args<K>> 
         ) {
            super()
         }

         static is<K> (ξ: Match<K>): ξ is Constr<K> {
            return ξ instanceof Constr
         }

         static make<K> (cases: FiniteMap<string, Traced.Args<K> | Args<K>>): Constr<K> {
            return make(Constr, cases)
         }
      }

      export class Fun<K> extends Match<K> {
         constructor (
            public f: Value.Closure | Value.PrimOp,
            public κ: K
         ) {
            super()
         }

         static is<K> (ξ: Match<K>): ξ is Fun<K> {
            return ξ instanceof Fun
         }

         static make<K> (f: Value.Closure | Value.PrimOp, κ: K): Fun<K> {
            return make<Fun<K>>(Fun, f, κ)
         }
      }

      export class Var<K> extends Match<K> {
         constructor (
            public x: Lex.Var,
            public v: Value | null,
            public κ: K
         ) {
            super()
         }

         static is<K> (ξ: Match<K>): ξ is Var<K> {
            return ξ instanceof Var
         }

         static make<K> (x: Lex.Var, v: Value | null, κ: K): Var<K> {
            return make<Var<K>>(Var, x, v, κ)
         }
      }
   }

   export class Trace extends VersionedObject<Runtime<Expr>> {
      __Trace_Trace (): void {
         // discriminator
      }
   }
   
   export class App extends Trace {
      func: Traced
      arg: Traced
      body: Trace | null

      constructor_ (func: Traced, arg: Traced, body: Trace | null): void {
         this.func = func
         this.arg = arg
         this.body = body
      }

      static at (k: Runtime<Expr>, func: Traced, arg: Traced, body: Trace | null): App {
         return at(k, App, func, arg, body)
      }
   }

   // Not the same as ⊥ (null); we distinguish information about an absence from the absence of information.
   export class Empty extends Trace {
      constructor_ (): void {
      }

      static at (k: Runtime<Expr>): Empty {
         return at(k, Empty)
      }
   }

   export class Let extends Trace {
      tu: Traced
      σ: Trie.Var<Traced>
      t: Trace | null

      constructor_ (tu: Traced, σ: Trie.Var<Traced>, t: Trace | null): void {
         this.tu = tu
         this.σ = σ
         this.t = t
      }

      static at (k: Runtime<Expr>, tu: Traced, σ: Trie.Var<Traced>, t: Trace | null): Let {
         return at(k, Let, tu, σ, t)
      }
   }

   export class RecDef extends VersionedObject<Runtime<Expr.RecDef>> {
      x: Lex.Var
      tv: Traced

      constructor_ (x: Lex.Var, tv: Traced): void {
         this.x = x
         this.tv = tv
      }
   
      static at (i: Runtime<Expr.RecDef>, x: Lex.Var, tv: Traced): RecDef {
         return at(i, RecDef, x, tv)
      }
   }

   // Continuation here should really be a trace, not a traced value.
   export class LetRec extends Trace {
      δ: List<RecDef>
      tv: Traced
   
      constructor_ (δ: List<RecDef>, tv: Traced): void {
         this.δ = δ
         this.tv = tv
      }

      static at (k: Runtime<Expr>, δ: List<RecDef>, tv: Traced): LetRec {
         return at(k, LetRec, δ, tv)
      }
   }
   
   export class MatchAs extends Trace {
      tu: Traced
      σ: Trie<Traced>
      t: Trace | null

      constructor_ (tu: Traced, σ: Trie<Traced>, t: Trace | null): void {
         this.tu = tu
         this.σ = σ
         this.t = t
      }

      static at (k: Runtime<Expr>, tu: Traced, σ: Trie<Traced>, t: Trace | null): MatchAs {
         return at(k, MatchAs, tu, σ, t)
      }
   }

   export class PrimApp extends Trace {
      tv1: Traced
      opName: Lex.OpName
      tv2: Traced

      constructor_ (tv1: Traced, opName: Lex.OpName, tv2: Traced): void {
         this.tv1 = tv1
         this.opName = opName
         this.tv2 = tv2
      }

      static at (k: Runtime<Expr>, tv1: Traced, opName: Lex.OpName, tv2: Traced): PrimApp {
         return at(k, PrimApp, tv1, opName, tv2)
      }
   }

   export class Var extends Trace {
      x: Lex.Var
      t: Trace | null

      constructor_ (x: Lex.Var, t: Trace | null): void {
         this.x = x
         this.t = t
      }

      static at (k: Runtime<Expr>, x: Lex.Var, t: Trace | null): Var {
         return at(k, Var, x, t)
      }
   }
}
