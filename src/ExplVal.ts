import { Persistent, PersistentObject, at, make } from "./util/Persistent"
import { Annotated, Annotation } from "./Annotated"
import { List } from "./BaseTypes"
import { Env } from "./Env"
import { FiniteMap } from "./FiniteMap"
import { Expr, Lex } from "./Expr"
import { ExplId, ValId } from "./Eval"
import { UnaryOp } from "./Primitive"

import Trie = Expr.Trie

export type Expr = Expr.Expr
export type Value = Value.Value

export namespace Value {
   export abstract class Value extends Annotated implements PersistentObject {
      __tag: "Value.Value"
      abstract constructor_ (...args: Persistent[]): void
   }

   export class Closure extends Value {
      ρ: Env // ρ is _not_ closing for σ; need to extend with the bindings in δ
      δ: List<Expr.RecDef>
      σ: Trie<Expr>
   
      constructor_ (α: Annotation, ρ: Env, δ: List<Expr.RecDef>, σ: Trie<Expr>): void {
         this.α = α
         this.ρ = ρ
         this.δ = δ
         this.σ = σ
      }

      static at (k: ValId, α: Annotation, ρ: Env, δ: List<Expr.RecDef>, σ: Trie<Expr>): Closure {
         return at(k, Closure, α, ρ, δ, σ)
      }
   }

   export abstract class Prim extends Value {
      __subsubtag: "Value.Prim"
   }
   
   export class ConstInt extends Prim {
      val: number

      constructor_ (α: Annotation, val: number): void {
         this.α = α
         this.val = val
      }
   
      static at (k: ValId, α: Annotation, val: number): ConstInt {
         return at(k, ConstInt, α, val)
      }

      toString (): string {
         return `${this.val}`
      }
   }
   
   export class ConstStr extends Prim {
      val: string

      constructor_ (α: Annotation, val: string): void {
         this.α = α
         this.val = val
      }
   
      static at (k: ValId, α: Annotation, val: string): ConstStr {
         return at(k, ConstStr, α, val)
      }

      toString (): string {
         return `"${this.val}"`
      }
   }
   
   export class Constr extends Value {
      ctr: Lex.Ctr
      args: List<ExplVal>

      constructor_ (α: Annotation, ctr: Lex.Ctr, args: List<ExplVal>): void {
         this.α = α
         this.ctr = ctr
         this.args = args
      }
   
      static at (k: ValId, α: Annotation, ctr: Lex.Ctr, args: List<ExplVal>): Constr {
         return at(k, Constr, α, ctr, args)
      }
   }

   export class PrimOp extends Value {
      op: UnaryOp

      constructor_ (α: Annotation, op: UnaryOp): void {
         this.α = α
         this.op = op
      }
   
      static at (k: ValId, α: Annotation, op: UnaryOp): PrimOp {
         return at(k, PrimOp, α, op)
      }
   }
}

export class ExplVal implements PersistentObject {
   ρ: Env // needed for uneval
   t: Expl
   v: Value

   constructor_ (
      ρ: Env,
      t: Expl,
      v: Value
   ) {
      this.ρ = ρ
      this.t = t
      this.v = v
   }

   static make (ρ: Env, t: Expl, v: Value): ExplVal {
      return make(ExplVal, ρ, t, v)
   }
}

export type Expl = ExplVal.Expl
export type Expl̊ = Expl | null

export namespace ExplVal {
   export class ExplValMatch<K> implements PersistentObject {
      t: Expl̊ // null iff ξ represents a dead branch
      ξ: Match<K>

   constructor_ (t: Expl̊, ξ: Match<K>) {
         this.t = t
         this.ξ = ξ
      }

      static make<K> (t: Expl̊, ξ: Match<K>): ExplValMatch<K> {
         return make(ExplValMatch, t, ξ)
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
            tξ: ExplValMatch<K>

            constructor_ (tξ: ExplValMatch<K>) {
               this.tξ = tξ
            }
   
            static is<K> (Ψ: Args<K>): Ψ is Next<K> {
               return Ψ instanceof Next
            }
   
            static make<K> (tξ: ExplValMatch<K>): Next<K> {
               return make(Next, tξ)
            }
         }
      }

      export abstract class Match<K> implements PersistentObject {
         __tag: "Match.Match"
         abstract constructor_ (...args: Persistent[]): void
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
         v: Value
         κ: K

         constructor_ (x: Lex.Var, v: Value, κ: K) {
            this.x = x
            this.v = v
            this.κ = κ
         }

         static is<K extends Persistent> (ξ: Match<K>): ξ is Var<K> {
            return ξ instanceof Var
         }

         static make<K extends Persistent> (x: Lex.Var, v: Value, κ: K): Var<K> {
            return make(Var, x, v, κ) as Var<K>
         }
      }
   }

   export abstract class Expl implements PersistentObject, Expr.Kont<Expl> {
      __tag: "Expl.Expl"
      abstract constructor_ (...args: Persistent[]): void
   }

   export class App extends Expl {
      func: ExplVal
      arg: ExplVal
      ρ: Env         // environment extension derived from matching argument, for uneval
      body: ExplVal  // technically Expl would suffice, but for uneval we want environment

      constructor_ (func: ExplVal, arg: ExplVal, ρ: Env, body: ExplVal): void {
         this.func = func
         this.arg = arg
         this.ρ = ρ
         this.body = body
      }

      static at (k: ExplId, func: ExplVal, arg: ExplVal, ρ: Env, body: ExplVal): App {
         return at(k, App, func, arg, ρ, body)
      }
   }

   export class UnaryApp extends Expl {
      func: ExplVal
      arg: ExplVal

      constructor_ (func: ExplVal, arg: ExplVal): void {
         this.func = func
         this.arg = arg
      }

      static at (k: ExplId, func: ExplVal, arg: ExplVal): App {
         return at(k, App, func, arg)
      }
   }

   export class Empty extends Expl {
      constructor_ (): void {
      }

      static at (k: ExplId): Empty {
         return at(k, Empty)
      }
   }

   export class Let extends Expl {
      tu: ExplVal
      σ: Expr.Trie.Var<ExplVal> // technically Expl would suffice, but for uneval we want environment

      constructor_ (tu: ExplVal, σ: Expr.Trie.Var<ExplVal>): void {
         this.tu = tu
         this.σ = σ
      }

      static at (k: ExplId, tu: ExplVal, σ: Expr.Trie.Var<ExplVal>): Let {
         return at(k, Let, tu, σ)
      }
   }

   export class LetRec extends Expl {
      δ: List<Expr.RecDef>
      tv: ExplVal
   
      constructor_ (δ: List<Expr.RecDef>, tv: ExplVal): void {
         this.δ = δ
         this.tv = tv
      }

      static at (k: ExplId, δ: List<Expr.RecDef>, tv: ExplVal): LetRec {
         return at(k, LetRec, δ, tv)
      }
   }
   
   export class MatchAs extends Expl {
      tu: ExplVal
      σ: Expr.Trie<Expr>
      tv: ExplVal // technically Expl would suffice, but for uneval we want environment

      constructor_ (tu: ExplVal, σ: Expr.Trie<Expr>, tv: ExplVal): void {
         this.tu = tu
         this.σ = σ
         this.tv = tv
      }

      static at (k: ExplId, tu: ExplVal, σ: Expr.Trie<Expr>, tv: ExplVal): MatchAs {
         return at(k, MatchAs, tu, σ, tv)
      }
   }

   export class BinaryApp extends Expl {
      tv1: ExplVal
      opName: Lex.OpName
      tv2: ExplVal

      constructor_ (tv1: ExplVal, opName: Lex.OpName, tv2: ExplVal): void {
         this.tv1 = tv1
         this.opName = opName
         this.tv2 = tv2
      }

      static at (k: ExplId, tv1: ExplVal, opName: Lex.OpName, tv2: ExplVal): BinaryApp {
         return at(k, BinaryApp, tv1, opName, tv2)
      }
   }

   export class Var extends Expl {
      x: Lex.Var
      t: Expl

      constructor_ (x: Lex.Var, t: Expl): void {
         this.x = x
         this.t = t
      }

      static at (k: ExplId, x: Lex.Var, t: Expl): Var {
         return at(k, Var, x, t)
      }
   }
}
