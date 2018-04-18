import { __check, absurd, make } from "./util/Core"
import { JoinSemilattice, eq } from "./util/Ord"
import { Lexeme } from "./util/parse/Core"
import { List } from "./BaseTypes"
import { Env } from "./Env"
import { FiniteMap, unionWith } from "./FiniteMap"
import { Eval } from "./Eval"
import { UnaryOp } from "./Primitive"
import { ExternalObject, VersionedObject, PersistentObject, create } from "./Runtime"

// Constants used for parsing, and also for toString() implementations.
export namespace str {
   export const arrow: string = "→"
   export const as: string = "as"
   export const equals: string = "="
   export const fun: string = "fun"
   export const in_: string = "in"
   export const let_: string = "let"
   export const letRec: string = "letrec"
   export const match: string = "match"
   export const parenL: string = "("
   export const parenR: string = ")"
   export const quotes: string = '"'
}

export namespace Lex {
   // With purely structural typing, these lexeme classes are identical, not just isomorphic. This
   // mostly sucks in a class-oriented languages like JavaScript, so we add dummy discriminator methods.

   export class Ctr extends Lexeme {
      constructor(str: string) {
         super(str)
      }

      __Ctr(): void {
         // discriminator
      }
   }

   export class IntLiteral extends Lexeme {
      constructor(str: string) {
         super(str)
      }

      toNumber(): number {
         return parseInt(this.str)
      }
   }

   export class Keyword extends Lexeme {
      constructor(str: string) {
         super(str)
      }
   }

   // The name of a primitive operation, such as * or +, where that name is /not/ a standard identifier.
   // Other uses of primitive operations are treated as variables.
   export class OpName extends Lexeme {
      constructor(str: string) {
         super(str)
      }

      __OpName(): void {
         // discriminator
      }
   }

   export class StringLiteral extends Lexeme {
      constructor(str: string) {
         super(str)
      }

      toString(): string {
         return str.quotes + this.str + str.quotes
      }
   }

   export class Var extends Lexeme {
      constructor(str: string) {
         super(str)
      }

      __Var(): void {
         // discriminator
      }
   }
}

export type Value = Value.Value

export namespace Value {
   export class Value extends VersionedObject {
      __Value(): void {
         // discriminator
      }
   }

   export class Closure extends Value {
      ρ: Env
      σ: Trie<Traced>
   
      static at (α: PersistentObject, ρ: Env, σ: Trie<Traced>): Closure {
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

export type Expr = Expr.Expr

export namespace Expr {
   export class Expr extends VersionedObject<ExternalObject> {
      __Expr(): void {
         // discriminator
      }
   }

   export class App extends Expr {
      func: Expr
      arg: Expr

      static at (i: ExternalObject, func: Expr, arg: Expr): App {
         const this_: App = create(i, App)
         this_.func = func
         this_.arg = arg
         this_.__version()
         return this_
      }
   }

   export class ConstInt extends Expr {
      val: number
   
      static at (i: ExternalObject, val: number): ConstInt {
         const this_: ConstInt = create(i, ConstInt)
         this_.val = __check(val, x => !Number.isNaN(x))
         this_.__version()
         return this_
      }
   }
   
   export class ConstStr extends Expr {
      val: string
   
      static at (i: ExternalObject, val: string): ConstStr {
         const this_: ConstStr = create(i, ConstStr)
         this_.val = val
         this_.__version()
         return this_
      }
   }
   
   export class Constr extends Expr {
      ctr: Lex.Ctr
      args: List<Expr>
   
      static at (i: ExternalObject, ctr: Lex.Ctr, args: List<Expr>): Constr {
         const this_: Constr = create(i, Constr)
         this_.ctr = ctr
         this_.args = args
         this_.__version()
         return this_
      }
   }

   export class Fun extends Expr {
      σ: Trie<Expr>

      static at (i: ExternalObject, σ: Trie<Expr>): Fun {
         const this_: Fun = create(i, Fun)
         this_.σ = σ
         this_.__version()
         return this_
      }
   }

   // A let is simply a match where the trie is a variable trie.
   export class Let extends Expr {
      e: Expr
      σ: Trie.Var<Expr>

      static at (i: ExternalObject, e: Expr, σ: Trie.Var<Expr>): Let {
         const this_: Let = create(i, Let)
         this_.e = e
         this_.σ = σ
         this_.__version()
         return this_
      }
   }

   export class PrimOp extends Expr {
      op: UnaryOp

      static at (i: ExternalObject, op: UnaryOp): PrimOp {
         const this_: PrimOp = create(i, PrimOp)
         this_.op = op
         this_.__version()
         return this_
      }
   }

   export class RecDef extends VersionedObject<ExternalObject> {
      x: Lex.Var
      e: Expr
   
      static at (α: ExternalObject, x: Lex.Var, e: Expr): RecDef {
         const this_: RecDef = create(α, RecDef)
         this_.x = x
         this_.e = e
         this_.__version()
         return this_
      }
   }

   export class LetRec extends Expr {
      δ: List<RecDef>
      e: Expr

      static at (i: ExternalObject, δ: List<RecDef>, e: Expr): LetRec {
         const this_: LetRec = create(i, LetRec)
         this_.δ = δ
         this_.e = e
         this_.__version()
         return this_
      }
   }

   export class MatchAs extends Expr {
      e: Expr
      σ: Trie<Expr>
   
      static at (i: ExternalObject, e: Expr, σ: Trie<Expr>): MatchAs {
         const this_: MatchAs = create(i, MatchAs)
         this_.e = e
         this_.σ = σ
         this_.__version()
         return this_
      }
   }

   export class PrimApp extends Expr {
      e1: Expr
      opName: Lex.OpName
      e2: Expr

      static at (i: ExternalObject, e1: Expr, opName: Lex.OpName, e2: Expr): PrimApp {
         const this_: PrimApp = create(i, PrimApp)
         this_.e1 = e1
         this_.opName = opName
         this_.e2 = e2
         this_.__version()
         return this_
      }
   }

   export class Var extends Expr {
      x: Lex.Var
   
      static at (i: ExternalObject, x: Lex.Var): Var {
         const this_: Var = create(i, Var)
         this_.x = x
         this_.__version()
         return this_
      }
   }
}

// Can these be interned, rather than versioned? Just realised interning objects with null fields won't respect 
// the LVar semantics :-o
export class Traced<T extends Value = Value> extends VersionedObject<Eval.Evaluand> {
   trace: Trace | null
   val: T | null

   static at <T extends Value> (k: Eval.Evaluand, trace: Trace | null, val: T | null): Traced<T> {
      const this_: Traced<T> = create<Eval.Evaluand, Traced<T>>(k, Traced)
      this_.trace = trace
      this_.val = val
      this_.__version()
      return this_
   }
}

// Tries are persistent but not versioned, as per the formalism.
export type Trie<T extends PersistentObject | null> = Trie.Trie<T>

export namespace Trie {
   export class Trie<T extends PersistentObject | null> 
      extends PersistentObject implements JoinSemilattice<Trie<T>> {
      join (σ: Trie<T>): Trie<T> {
         return join(this, σ)
      }
   }

   export class Prim<T extends PersistentObject | null> extends Trie<T> {
      body: T
   }

   export class ConstInt<T extends PersistentObject | null> extends Prim<T> {
      static is<T extends PersistentObject | null> (σ: Trie<T>): σ is ConstInt<T> {
         return σ instanceof ConstInt
      }

      static make <T extends PersistentObject | null> (body: T): ConstInt<T> {
         const this_: ConstInt<T> = make<ConstInt<T>>(ConstInt, body)
         this_.body = body
         return this_
      }
   }

   export class ConstStr<T extends PersistentObject | null> extends Prim<T> {
      static is<T extends PersistentObject | null> (σ: Trie<T>): σ is ConstStr<T> {
         return σ instanceof ConstStr
      }

      static make <T extends PersistentObject | null> (body: T): ConstStr<T> {
         const this_: ConstStr<T> = make<ConstStr<T>>(ConstStr, body)
         this_.body = body
         return this_
      }
   }

   export class Constr<T extends PersistentObject | null> extends Trie<T> {
      cases: FiniteMap<string, T>

      static is<T extends PersistentObject | null> (σ: Trie<T>): σ is Constr<T> {
         return σ instanceof Constr
      }

      static make <T extends PersistentObject | null> (cases: FiniteMap<string, T>): Constr<T> {
         const this_: Constr<T> = make<Constr<T>>(Constr, cases)
         this_.cases = cases
         return this_
      }
   }

   export class Var<T extends PersistentObject | null> extends Trie<T> {
      x: Lex.Var
      body: T

      static is<T extends PersistentObject | null> (σ: Trie<T>): σ is Var<T> {
         return σ instanceof Var
      }

      static make <T extends PersistentObject | null> (x: Lex.Var, body: T): Var<T> {
         const this_: Var<T> = make<Var<T>>(Var, x, body)
         this_.x = x
         this_.body = body
         return this_
      }
   }

   export class Fun<T extends PersistentObject | null> extends Trie<T> {
      body: T

      static is<T extends PersistentObject | null> (σ: Trie<T>): σ is Fun<T> {
         return σ instanceof Fun
      }

      static make <T extends PersistentObject | null> (body: T): Fun<T> {
         const this_: Fun<T> = make<Fun<T>>(Fun, body)
         this_.body = body
         return this_
      }
   }

   export function join<T extends JoinSemilattice<T> & PersistentObject> (σ: Trie<T>, τ: Trie<T>): Trie<T> {
      if (σ === null) {
         return τ
      } else
      if (τ === null) {
         return σ
      } else
      if (Fun.is(σ) && Fun.is(τ)) {
         return Fun.make(σ.body.join(τ.body))
      } else
      if (Var.is(σ) && Var.is(τ) && eq(σ.x, τ.x)) {
         return Var.make(σ.x, σ.body.join(τ.body))
      } else
      if (Constr.is(σ) && Constr.is(τ)) {
         return Constr.make<T>(unionWith(σ.cases, τ.cases, (x, y) => x.join(y)))
      } else {
         return absurd("Undefined join.", σ, τ)
      }
   }
}

export type Trace = Trace.Trace

export namespace Trace {
   export class Trace extends VersionedObject<Eval.Evaluand> {
      __Trace(): void {
         // discriminator
      }
   }
   
   export class App extends Trace {
      func: Traced
      arg: Traced
      body: Trace | null

      static at (k: Eval.Evaluand, func: Traced, arg: Traced, body: Trace | null): App {
         const this_: App = create(k, App)
         this_.func = func
         this_.arg = arg
         this_.body = body
         this_.__version()
         return this_
      }
   }

   // Not to be confused with ⊥ (null); this is information about an absence, not the absence of information.
   export class Empty extends Trace {
      static at (k: Eval.Evaluand): Empty {
         const this_: Empty = create(k, Empty)
         this_.__version()
         return this_
      }
   }

   export class Let extends Trace {
      tu: Traced
      σ: Trie.Var<Traced>
      t: Trace

      __Let (): void {
         // discriminator
      }

      static at (k: Eval.Evaluand, tu: Traced, σ: Trie.Var<Traced>, t: Trace): Let {
         const this_: Let = create(k, Let)
         this_.tu = tu
         this_.σ = σ
         this_.t = t
         this_.__version()
         return this_
      }
   }

   export class RecDef extends VersionedObject<Eval.Evaluand> {
      x: Lex.Var
      tv: Traced
   
      static at (i: Eval.Evaluand, x: Lex.Var, tv: Traced): RecDef {
         const this_: RecDef = create(i, RecDef)
         this_.x = x
         this_.tv = tv
         this_.__version()
         return this_
      }
   }

   export class LetRec extends Trace {
      δ: List<RecDef>
      t: Trace
   
      static at (k: Eval.Evaluand, δ: List<RecDef>, t: Trace): LetRec {
         const this_: LetRec = create(k, LetRec)
         this_.δ = δ
         this_.t = t
         this_.__version()
         return this_
      }
   }
   
   export class MatchAs extends Trace {
      tu: Traced
      σ: Trie<Traced>
      t: Trace | null

      __Match (): void {
         // discriminator
      }

      static at (k: Eval.Evaluand, tu: Traced, σ: Trie<Traced>,  t: Trace | null): MatchAs {
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

      static at (k: Eval.Evaluand, tv1: Traced, opName: Lex.OpName, tv2: Traced): PrimApp {
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

      static at (k: Eval.Evaluand, x: Lex.Var, t: Trace | null): Var {
         const this_: Var = create(k, Var)
         this_.x = x
         this_.t = t
         this_.__version()
         return this_
      }
   }
}
