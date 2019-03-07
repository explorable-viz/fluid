import { Persistent, PersistentObject, at, make } from "./util/Persistent"
import { List } from "./BaseTypes"
import { Env } from "./Env"
import { FiniteMap } from "./FiniteMap"
import { Expr, Lex } from "./Expr"
import { TraceId, ValId } from "./Eval"
import { UnaryOp } from "./Primitive"

import Trie = Expr.Trie

export type Expr = Expr.Expr
export type Value = Value.Value
export type Value̊ = Value | null

export namespace Value {
   export abstract class Value implements PersistentObject {
      __tag: "Value.Value"
      abstract constructor_ (...args: Persistent[]): void
   }

   export class Closure extends Value {
      ρ: Env // ρ is _not_ closing for σ; need to extend with the bindings in δ
      δ: List<Expr.RecDef>
      σ: Trie<Expr>
   
      constructor_ (ρ: Env, δ: List<Expr.RecDef>, σ: Trie<Expr>): void {
         this.ρ = ρ
         this.δ = δ
         this.σ = σ
      }

      static at (k: ValId, ρ: Env, δ: List<Expr.RecDef>, σ: Trie<Expr>): Closure {
         return at(k, Closure, ρ, δ, σ)
      }
   }

   export abstract class Prim extends Value {
      __subsubtag: "Value.Prim"
   }
   
   export class ConstInt extends Prim {
      val: number

      constructor_ (val: number): void {
         this.val = val
      }
   
      static at (k: ValId, val: number): ConstInt {
         return at(k, ConstInt, val)
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
   
      static at (k: ValId, val: string): ConstStr {
         return at(k, ConstStr, val)
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
   
      static at (k: ValId, ctr: Lex.Ctr, args: List<Traced>): Constr {
         return at(k, Constr, ctr, args)
      }
   }

   export class PrimOp extends Value {
      op: UnaryOp

      constructor_ (op: UnaryOp): void {
         this.op = op
      }
   
      static at (k: ValId, op: UnaryOp): PrimOp {
         return at(k, PrimOp, op)
      }
   }
}

// Called ExplVal in the formalism.
export class Traced implements PersistentObject {
   t: Trace
   v: Value

   constructor_ (
      t: Trace,
      v: Value
   ) {
      this.t = t
      this.v = v
   }

   static make (t: Trace, v: Value): Traced {
      return make(Traced, t, v)
   }
}

export type Trace = Traced.Trace
export type Trace̊ = Trace | null

export namespace Traced {
   export class TracedMatch<K> implements PersistentObject {
      t: Trace̊ // null iff ξ represents a dead branch
      ξ: Match<K>

   constructor_ (t: Trace̊, ξ: Match<K>) {
         this.t = t
         this.ξ = ξ
      }

      static make<K> (t: Trace̊, ξ: Match<K>): TracedMatch<K> {
         return make(TracedMatch, t, ξ)
      }
   }

   export type Match<K> = Match.Match<K>

   // A trie which has been matched (executed) to a depth of at least one.
   export namespace Match {
      export type Args<K extends Expr.Kont<K>> = Args.Args<K>

      export namespace Args {
         export abstract class Args<K> implements Expr.Kont<Args<K>> {
            __tag: "Match.Args"
            abstract constructor_ (...args: Persistent[]): void
         }
   
         export class End<K extends Persistent> extends Args<K> {
            κ: K

            constructor_ (κ: K) {
               this.κ = κ
            }
   
            static is<K extends Persistent> (Ψ: Args<K>): Ψ is End<K> {
               return Ψ instanceof End
            }
   
            static make<K extends Persistent> (κ: K): End<K> {
               return make(End, κ) as End<K>
            }
         }
   
         export class Next<K> extends Args<K> {
            tξ: TracedMatch<K>

            constructor_ (tξ: TracedMatch<K>) {
               this.tξ = tξ
            }
   
            static is<K> (Ψ: Args<K>): Ψ is Next<K> {
               return Ψ instanceof Next
            }
   
            static make<K> (tξ: TracedMatch<K>): Next<K> {
               return make(Next, tξ)
            }
         }
      }

      export abstract class Match<K> implements PersistentObject {
         __tag: "Match.Match"
         abstract constructor_ (...args: Persistent[]): void // TS requires duplicate def
      }

      // Exactly one branch will be live (i.e. an instanceof Match.Args rather than Trie.Args).
      export class Constr<K extends Expr.Kont<K>> extends Match<K> {
         cases: FiniteMap<string, Expr.Args<K> | Args<K>> 

         constructor_ (cases: FiniteMap<string, Expr.Args<K> | Args<K>>) {
            this.cases = cases
         }

         static is<K extends Expr.Kont<K>> (ξ: Match<K>): ξ is Constr<K> {
            return ξ instanceof Constr
         }

         static make<K extends Expr.Kont<K>> (cases: FiniteMap<string, Expr.Args<K> | Args<K>>): Constr<K> {
            return make(Constr, cases)
         }
      }

      export class Fun<K extends Persistent> extends Match<K> {
         f: Value.Closure | Value.PrimOp
         κ: K
   
         constructor_ (f: Value.Closure | Value.PrimOp, κ: K) {
            this.f = f
            this.κ = κ
         }

         static is<K extends Persistent> (ξ: Match<K>): ξ is Fun<K> {
            return ξ instanceof Fun
         }

         static make<K extends Persistent> (f: Value.Closure | Value.PrimOp, κ: K): Fun<K> {
            return make(Fun, f, κ) as Fun<K>
         }
      }

      export class Var<K extends Persistent> extends Match<K> {
         x: Lex.Var
         v: Value̊
         κ: K

         constructor_ (x: Lex.Var, v: Value̊, κ: K) {
            this.x = x
            this.v = v
            this.κ = κ
         }

         static is<K extends Persistent> (ξ: Match<K>): ξ is Var<K> {
            return ξ instanceof Var
         }

         static make<K extends Persistent> (x: Lex.Var, v: Value̊, κ: K): Var<K> {
            return make(Var, x, v, κ) as Var<K>
         }
      }
   }

   export abstract class Trace implements PersistentObject, Expr.Kont<Trace> {
      __tag: "Trace.Trace"
      abstract constructor_ (...args: Persistent[]): void
   }

   export class App extends Trace {
      func: Traced
      arg: Traced
      body: Trace

      constructor_ (func: Traced, arg: Traced, body: Trace): void {
         this.func = func
         this.arg = arg
         this.body = body
      }

      static at (k: TraceId, func: Traced, arg: Traced, body: Trace): App {
         return at(k, App, func, arg, body)
      }
   }

   export class UnaryApp extends Trace {
      func: Traced
      arg: Traced

      constructor_ (func: Traced, arg: Traced): void {
         this.func = func
         this.arg = arg
      }

      static at (k: TraceId, func: Traced, arg: Traced): App {
         return at(k, App, func, arg)
      }
   }

   // Not the same as ⊥ (null); we distinguish information about an absence from the absence of information.
   export class Empty extends Trace {
      constructor_ (): void {
      }

      static at (k: TraceId): Empty {
         return at(k, Empty)
      }
   }

   export class Let extends Trace {
      tu: Traced
      σ: Expr.Trie.Var<Trace>

      constructor_ (tu: Traced, σ: Expr.Trie.Var<Trace>): void {
         this.tu = tu
         this.σ = σ
      }

      static at (k: TraceId, tu: Traced, σ: Expr.Trie.Var<Trace>): Let {
         return at(k, Let, tu, σ)
      }
   }

   // Continuation here should really be a trace, not a traced value.
   export class LetRec extends Trace {
      δ: List<Expr.RecDef>
      tv: Traced
   
      constructor_ (δ: List<Expr.RecDef>, tv: Traced): void {
         this.δ = δ
         this.tv = tv
      }

      static at (k: TraceId, δ: List<Expr.RecDef>, tv: Traced): LetRec {
         return at(k, LetRec, δ, tv)
      }
   }
   
   export class MatchAs extends Trace {
      tu: Traced
      σ: Expr.Trie<Expr>
      t: Trace

      constructor_ (tu: Traced, σ: Expr.Trie<Expr>, t: Trace): void {
         this.tu = tu
         this.σ = σ
         this.t = t
      }

      static at (k: TraceId, tu: Traced, σ: Expr.Trie<Expr>, t: Trace): MatchAs {
         return at(k, MatchAs, tu, σ, t)
      }
   }

   export class BinaryApp extends Trace {
      tv1: Traced
      opName: Lex.OpName
      tv2: Traced

      constructor_ (tv1: Traced, opName: Lex.OpName, tv2: Traced): void {
         this.tv1 = tv1
         this.opName = opName
         this.tv2 = tv2
      }

      static at (k: TraceId, tv1: Traced, opName: Lex.OpName, tv2: Traced): BinaryApp {
         return at(k, BinaryApp, tv1, opName, tv2)
      }
   }

   export class Var extends Trace {
      x: Lex.Var
      t: Trace

      constructor_ (x: Lex.Var, t: Trace): void {
         this.x = x
         this.t = t
      }

      static at (k: TraceId, x: Lex.Var, t: Trace): Var {
         return at(k, Var, x, t)
      }
   }
}
