import { __nonNull, assert } from "./util/Core"
import { Persistent, PersistentObject, make } from "./util/Persistent"
import { at } from "./util/Versioned"
import { Annotated, Annotation } from "./Annotated"
import { List } from "./BaseTypes"
import { Env } from "./Env"
import { FiniteMap } from "./FiniteMap"
import { Expr, Kont, Lex } from "./Expr"
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
   }

   export function closure (k: ValId, α: Annotation, ρ: Env, δ: List<Expr.RecDef>, σ: Trie<Expr>): Closure {
      return at(k, Closure, α, ρ, δ, σ)
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
   
      toString (): string {
         return `${this.val}`
      }
   }
   
   export function constInt (k: ValId, α: Annotation, val: number): ConstInt {
      return at(k, ConstInt, α, val)
   }

   export class ConstStr extends Prim {
      val: string

      constructor_ (α: Annotation, val: string): void {
         this.α = α
         this.val = val
      }
   
      toString (): string {
         return `"${this.val}"`
      }
   }
   
   export function constStr (k: ValId, α: Annotation, val: string): ConstStr {
      return at(k, ConstStr, α, val)
   }

   export class Constr extends Value {
      ctr: Lex.Ctr
      args: List<ExplVal>

      constructor_ (α: Annotation, ctr: Lex.Ctr, args: List<ExplVal>): void {
         this.α = α
         this.ctr = ctr
         this.args = args
      }
   }
   
   export function constr (k: ValId, α: Annotation, ctr: Lex.Ctr, args: List<ExplVal>): Constr {
      return at(k, Constr, α, ctr, args)
   }

   export class PrimOp extends Value {
      op: UnaryOp

      constructor_ (α: Annotation, op: UnaryOp): void {
         this.α = α
         this.op = op
      }
   }
   
   export function primOp (k: ValId, α: Annotation, op: UnaryOp): PrimOp {
      return at(k, PrimOp, α, op)
   }
}

export class ExplVal implements PersistentObject, Kont<ExplVal> {
   __tag: "ExplVal"
   ρ: Env // needed for uneval
   t: Expl
   v: Value

   constructor_ (ρ: Env, t: Expl, v: Value) {
      this.ρ = ρ
      this.t = t
      this.v = v
   }
}

export function explVal (ρ: Env, t: Expl, v: Value): ExplVal {
   assert(!(t instanceof ExplVal.Var) || ρ.has(t.x.str))
   return make(ExplVal, ρ, t, v)
}

export type Match<K> = Match.Match<K>

// Tries which have been matched to a depth of at least one. Note that while tries are interned, matches are
// versioned; this is to allow unevaluation to recover the matched value.
export namespace Match {
   export class Plug<K extends Kont<K>, M extends Match<K>> implements PersistentObject {
      ξ: M    
      κ: K  // fills the single hole in ξ

      constructor_ (ξ: M, κ: K): void {
         this.ξ = ξ
         this.κ = κ
      }
   }

   export function plug<K extends Kont<K>, M extends Match<K>> (ξ: M, κ: K): Plug<K, M> {
      return make(Plug, ξ, κ) as Plug<K, M>
   }

   export type Args<K extends Kont<K>> = Args.Args<K>

   export namespace Args {
      export class Plug<K extends Kont<K>, M extends Args<K>> implements PersistentObject {
         Ψ: M  
         κ: K  // fills the single hole in Ψ
   
         constructor_ (Ψ: M, κ: K): void {
            this.Ψ = Ψ
            this.κ = κ
         }
      }
   
      export function plug<K extends Kont<K>, M extends Args<K>> (Ψ: M, κ: K): Plug<K, M> {
         return make(Plug, Ψ, κ) as Plug<K, M>
      }
   
      export abstract class Args<K extends Kont<K>> implements Kont<Args<K>> {
         __tag: "Match.Args"
         abstract constructor_ (...args: Persistent[]): void
         ρ: Env
      }

      export class End<K extends Kont<K>> extends Args<K> {
         constructor_ (ρ: Env): void {
            this.ρ = ρ
         }

         static is<K extends Kont<K>> (Ψ: Args<K>): Ψ is End<K> {
            return Ψ instanceof End
         }
      }

      export function end<K extends Kont<K>> (ρ: Env): End<K> {
         return make(End, ρ) as End<K>
      }

      export class Next<K extends Kont<K>> extends Args<K> {
         tξ: ExplMatch<K>
         Ψ: Args<K>

         constructor_ (ρ: Env, tξ: ExplMatch<K>, Ψ: Args<K>) {
            this.ρ = ρ
            this.tξ = tξ
            this.Ψ = Ψ
         }

         static is<K extends Kont<K>> (Ψ: Args<K>): Ψ is Next<K> {
            return Ψ instanceof Next
         }
      }

      export function next<K extends Kont<K>> (ρ: Env, tξ: ExplMatch<K>, Ψ: Args<K>): Next<K> {
         return make(Next, ρ, tξ, Ψ) as Next<K>
      }
   }

   // The environment ρ is completely determined by other properties of the match, but the convention is that all
   // properties are set externally via the constructor.
   export abstract class Match<K> implements PersistentObject {
      __tag: "Match.Match"
      abstract constructor_ (...args: Persistent[]): void
      ρ: Env 
   }

   // Exactly one branch will be live (i.e. an instanceof Match.Args rather than Trie.Args). Currently caches
   // the value originally matched, so a slice of it can be reconstructed at the right location; the alternative
   // would be to have match ids determined by the input value and trie (but that requires versioned tries).
   export class Constr<K extends Kont<K>> extends Match<K> {
      cases: FiniteMap<string, Expr.Args<K> | Args<K>> 
      v: Value.Constr

      constructor_ (ρ: Env, cases: FiniteMap<string, Expr.Args<K> | Args<K>>, v: Value.Constr) {
         this.ρ = ρ
         this.cases = cases
         this.v = v
      }

      static is<K extends Kont<K>> (ξ: Match<K>): ξ is Constr<K> {
         return ξ instanceof Constr
      }
   }

   export function constr<K extends Kont<K>> (ρ: Env, cases: FiniteMap<string, Expr.Args<K> | Args<K>>, v: Value.Constr): Constr<K> {
      return make(Constr, ρ, cases, v) as Constr<K>
   }

   export class Var<K extends Persistent> extends Match<K> {
      x: Lex.Var
      v: Value

      constructor_ (ρ: Env, x: Lex.Var, v: Value) {
         this.ρ = ρ
         this.x = x
         this.v = v
      }

      static is<K extends Persistent> (ξ: Match<K>): ξ is Var<K> {
         return ξ instanceof Var
      }
   }

   export function var_<K extends Persistent> (ρ: Env, x: Lex.Var, v: Value): Var<K> {
      return make(Var, ρ, x, v) as Var<K>
   }
}

export class ExplMatch<K extends Kont<K>> implements PersistentObject {
   ρ: Env // by analogy with ExplVal
   t: Expl
   ξ: Match.Match<K>

   constructor_ (ρ: Env, t: Expl, ξ: Match<K>) {
      this.ρ = ρ
      this.t = t
      this.ξ = ξ
   }
}

export function explMatch<K extends Kont<K>> (ρ: Env, t: Expl, ξ: Match<K>): ExplMatch<K> {
   return make(ExplMatch, ρ, t, ξ) as ExplMatch<K>
}

export type Expl = ExplVal.Expl

export namespace ExplVal {

   export abstract class Expl implements PersistentObject {
      __tag: "Expl.Expl"
      abstract constructor_ (...args: Persistent[]): void
   }

   export class App extends Expl {
      func: ExplVal                             // Expl would suffice, but for uneval we need address of function
      arg: ExplVal                              // Expl would suffice, but more uniform this way
      ρ_defs: Env                               // from closeDefs, for uneval
      ξtv: Match.Plug<ExplVal, Match<ExplVal>>  // technically Expl would suffice, but for uneval we want environment

      constructor_ (func: ExplVal, arg: ExplVal, ρ_defs: Env, ξtv: Match.Plug<ExplVal, Match<ExplVal>>): void {
         this.func = func
         this.arg = arg
         this.ρ_defs = ρ_defs
         this.ξtv = ξtv
      }
   }

   export function app (k: ExplId, func: ExplVal, arg: ExplVal, ρ_defs: Env, ξtv: Match.Plug<ExplVal, Match<ExplVal>>): App {
      return at(k, App, func, __nonNull(arg), __nonNull(ρ_defs), ξtv)
   }

   export class UnaryApp extends Expl {
      func: ExplVal
      arg: ExplVal

      constructor_ (func: ExplVal, arg: ExplVal): void {
         this.func = func
         this.arg = arg
      }
   }

   export function unaryApp (k: ExplId, func: ExplVal, arg: ExplVal): UnaryApp {
      return at(k, UnaryApp, func, arg)
   }

   export class Empty extends Expl {
      constructor_ (): void {
      }
   }

   export function empty (k: ExplId): Empty {
      return at(k, Empty)
   }

   export class Let extends Expl {
      tu: ExplVal
      ξtv: Match.Plug<ExplVal, Match.Var<ExplVal>> // technically Expl would suffice, but for uneval we want environment

      constructor_ (tu: ExplVal, ξtv: Match.Plug<ExplVal, Match.Var<ExplVal>>): void {
         this.tu = tu
         this.ξtv = ξtv
      }
   }

   export function let_ (k: ExplId, tu: ExplVal, ξt: Match.Plug<ExplVal, Match.Var<ExplVal>>): Let {
      return at(k, Let, tu, ξt)
   }

   export class LetRec extends Expl {
      δ: List<Expr.RecDef>
      ρ_defs: Env      // from closeDefs, for uneval
      tv: ExplVal
   
      constructor_ (δ: List<Expr.RecDef>, ρ_defs: Env, tv: ExplVal): void {
         this.δ = δ
         this.ρ_defs = ρ_defs
         this.tv = tv
      }
   }

   export function letRec (k: ExplId, δ: List<Expr.RecDef>, ρ_defs: Env, tv: ExplVal): LetRec {
      return at(k, LetRec, δ, ρ_defs, tv)
   }

   export class MatchAs extends Expl {
      tu: ExplVal
      ξtv: Match.Plug<ExplVal, Match<ExplVal>>  // technically Expl would suffice, but for uneval we want environment

      constructor_ (tu: ExplVal, ξtv: Match.Plug<ExplVal, Match<ExplVal>>): void {
         this.tu = tu
         this.ξtv = ξtv
      }
   }

   export function matchAs (k: ExplId, tu: ExplVal, ξtv: Match.Plug<ExplVal, Match<ExplVal>>): MatchAs {
      return at(k, MatchAs, tu, ξtv)
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
   }

   export function binaryApp (k: ExplId, tv1: ExplVal, opName: Lex.OpName, tv2: ExplVal): BinaryApp {
      return at(k, BinaryApp, tv1, opName, tv2)
   }

   export class Var extends Expl {
      x: Lex.Var

      constructor_ (x: Lex.Var): void {
         this.x = x
      }
   }

   export function var_ (k: ExplId, x: Lex.Var): Var {
      return at(k, Var, x)
   }
}
