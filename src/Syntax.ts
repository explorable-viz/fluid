import { __check, absurd, make } from "./util/Core"
import { JoinSemilattice, eq } from "./util/Ord"
import { Lexeme } from "./util/parse/Core"
import { List } from "./BaseTypes"
import { Env } from "./Env"
import { FiniteMap, unionWith } from "./FiniteMap"
import { Runtime } from "./Eval"
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
   
   // DELETE ME.
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

// Plugs a value into its explanation.
export class Traced<V extends Value = Value> extends PersistentObject {
   t: Trace
   v: V | null

   static make <V extends Value> (t: Trace, v: V | null): Traced<V> {
      const this_: Traced<V> = make<Traced<V>>(Traced, t, v)
      this_.t = t
      this_.v = v
      return this_
   }
}

export type TrieBody<K extends PersistentObject | null> = K | Trie<K>

// Tries are persistent but not versioned, as per the formalism.
export type Trie<K extends PersistentObject | null> = Trie.Trie<K>

export namespace Trie {
   export class Trie<K extends PersistentObject | null> 
      extends PersistentObject implements JoinSemilattice<Trie<K>> {
      static is<K extends PersistentObject | null> (x: TrieBody<K>): x is Trie<K> {
         return x instanceof Trie
      }

      join (σ: Trie<K>): Trie<K> {
         return join(this, σ)
      }
   }

   export class Prim<K extends PersistentObject | null> extends Trie<K> {
      body: K
   }

   export class ConstInt<K extends PersistentObject | null> extends Prim<K> {
      static is<K extends PersistentObject | null> (σ: Trie<K>): σ is ConstInt<K> {
         return σ instanceof ConstInt
      }

      static make <K extends PersistentObject | null> (body: K): ConstInt<K> {
         const this_: ConstInt<K> = make<ConstInt<K>>(ConstInt, body)
         this_.body = body
         return this_
      }
   }

   export class ConstStr<K extends PersistentObject | null> extends Prim<K> {
      static is<K extends PersistentObject | null> (σ: Trie<K>): σ is ConstStr<K> {
         return σ instanceof ConstStr
      }

      static make <K extends PersistentObject | null> (body: K): ConstStr<K> {
         const this_: ConstStr<K> = make<ConstStr<K>>(ConstStr, body)
         this_.body = body
         return this_
      }
   }

   export class Constr<K extends PersistentObject | null> extends Trie<K> {
      cases: FiniteMap<string, TrieBody<K>>

      static is<K extends PersistentObject | null> (σ: Trie<K>): σ is Constr<K> {
         return σ instanceof Constr
      }

      static make <K extends PersistentObject | null> (cases: FiniteMap<string, TrieBody<K>>): Constr<K> {
         const this_: Constr<K> = make<Constr<K>>(Constr, cases)
         this_.cases = cases
         return this_
      }
   }

   export class Var<K extends PersistentObject | null> extends Trie<K> {
      x: Lex.Var
      body: K

      static is<K extends PersistentObject | null> (σ: Trie<K>): σ is Var<K> {
         return σ instanceof Var
      }

      static make <K extends PersistentObject | null> (x: Lex.Var, body: K): Var<K> {
         const this_: Var<K> = make<Var<K>>(Var, x, body)
         this_.x = x
         this_.body = body
         return this_
      }
   }

   export class Fun<K extends PersistentObject | null> extends Trie<K> {
      body: K

      static is<K extends PersistentObject | null> (σ: Trie<K>): σ is Fun<K> {
         return σ instanceof Fun
      }

      static make <K extends PersistentObject | null> (body: K): Fun<K> {
         const this_: Fun<K> = make<Fun<K>>(Fun, body)
         this_.body = body
         return this_
      }
   }

   export function join<K extends JoinSemilattice<K> & PersistentObject> (σ: Trie<K>, τ: Trie<K>): Trie<K> {
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
         return Constr.make(unionWith(σ.cases, τ.cases, (x: TrieBody<K>, y: TrieBody<K>): Trie<K> => join(x, y)))
      } else {
         return absurd("Undefined join.", σ, τ)
      }
   }
}

export type Trace = Trace.Trace

export namespace Trace {
   // A trace has a single hole, which can be plugged by pairing it with a value. 
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

   // Essentially ⊥ (null); we conflate information about an absence with the absence of information.
   export class Empty extends Trace {
      static at (k: Runtime<Expr>): Empty {
         const this_: Empty = create(k, Empty)
         this_.__version()
         return this_
      }
   }

   export class Constr extends Trace {
      cases: FiniteMap<string, TrieBody<K>>
   }

   export class Let extends Trace {
      tu: Traced
      σ: Trie.Var<Traced>
      t: Trace | null

      __Let (): void {
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

   // Continuation here should really be a trace, not a traced value..
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

      __Match (): void {
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
