import { __check, assert, make } from "./util/Core"
import { unionWith } from "./util/Map"
import { JoinSemilattice, eq } from "./util/Ord"
import { Lexeme } from "./util/parse/Core"
import { Env } from "./Env"
import { Eval } from "./Eval"
import { PrimBody } from "./Primitive"
import { VersionedObject, RawId, create } from "./Runtime"

// Fresh keys represent inputs to the system.
export const ν: () => Expr.ExprId =
   (() => {
      let count: number = 0
      return () => {
         return Expr.ExprId.make(count++)
      }
   })()

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

export namespace Value {
   export class ValId {
      __ValId(): void {
         // discriminator
      }
   }

   export class Value extends VersionedObject<ValId> {
      __Value(): void {
         // discriminator
      }
   }

   export class Closure extends Value {
      ρ: Env
      δ: Expr.RecDefs
      func: Expr.Fun
   
      static at (α: ValId, ρ: Env, δ: Expr.RecDefs, func: Expr.Fun): Closure {
         const this_: Closure = create(α, Closure)
         this_.ρ = ρ
         this_.δ = δ
         this_.func = func
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
   
      static at (α: ValId, val: number): ConstInt {
         const this_: ConstInt = create(α, ConstInt)
         this_.val = val
         this_.__version()
         return this_
      }
   }
   
   export class ConstStr extends Prim {
      val: string
   
      static at (α: ValId, val: string): ConstStr {
         const this_: ConstStr = create(α, ConstStr)
         this_.val = val
         this_.__version()
         return this_
      }
   }
   
   export class Constr extends Value {
      ctr: Lex.Ctr
      args: Traced[]
   
      static at (α: ValId, ctr: Lex.Ctr, args: Traced[]): Constr {
         const this_: Constr = create(α, Constr)
         this_.ctr = ctr
         this_.args = args
         this_.__version()
         return this_
      }
   }

   // Primitive ops; see 0.4.4 release notes.
   export class PrimOp extends Value {
      name: string
      σ: Trie.Prim<PrimBody<any>>

      static at (α: ValId, name: string, σ: Trie.Prim<PrimBody<any>>): PrimOp {
         const this_: PrimOp = create(α, PrimOp)
         this_.name = name
         this_.σ = σ
         this_.__version()
         return this_
      }
   }
}

export namespace Expr {
   export class ExprId {
      id: RawId

      __ExprId(): void {
         // discriminator
      }
   
      static make (id: RawId): ExprId {
         const this_: ExprId = make(ExprId, id)
         this_.id = id
         return this_
      }
   }
      
   export class Expr extends VersionedObject<ExprId> {
      __Expr(): void {
         // discriminator
      }
   }

   export class App extends Expr {
      func: Expr
      arg: Expr

      static at (i: ExprId, func: Expr, arg: Expr): App {
         const this_: App = create(i, App)
         this_.func = func
         this_.arg = arg
         this_.__version()
         return this_
      }
   }

   export class ConstInt extends Expr {
      val: number
   
      static at (i: ExprId, val: number): ConstInt {
         const this_: ConstInt = create(i, ConstInt)
         this_.val = __check(val, x => !Number.isNaN(x))
         this_.__version()
         return this_
      }
   }
   
   export class ConstStr extends Expr {
      val: string
   
      static at (i: ExprId, val: string): ConstStr {
         const this_: ConstStr = create(i, ConstStr)
         this_.val = val
         this_.__version()
         return this_
      }
   }
   
   export class Constr extends Expr {
      ctr: Lex.Ctr
      args: Expr[]
   
      static at (i: ExprId, ctr: Lex.Ctr, args: Expr[]): Constr {
         const this_: Constr = create(i, Constr)
         this_.ctr = ctr
         this_.args = args
         this_.__version()
         return this_
      }
   }

   export class Fun extends Expr {
      σ: Trie.Trie<Expr>

      static at (i: ExprId, σ: Trie.Trie<Expr>): Fun {
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

      static at (i: ExprId, e: Expr, σ: Trie.Var<Expr>): Let {
         const this_: Let = create(i, Let)
         this_.e = e
         this_.σ = σ
         this_.__version()
         return this_
      }
   }

   export class RecDefId {
      i: ExprId

      __RecDefId(): void {
         // discriminator
      }
   
      static make (i: ExprId): RecDefId {
         const this_: RecDefId = make(RecDefId, i)
         this_.i = i
         return this_
      }
   }

   export class RecDef extends VersionedObject<RecDefId> {
      x: Lex.Var
      def: Fun
   
      static at (α: RecDefId, x: Lex.Var, def: Fun): RecDef {
         const this_: RecDef = create(α, RecDef)
         this_.x = x
         this_.def = def
         this_.__version()
         return this_
      }
   }

   // Interned rather than persistent.
   export abstract class RecDefs {
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
   
   export class LetRec extends Expr {
      δ: RecDefs
      e: Expr

      static at (i: ExprId, δ: RecDefs, e: Expr): LetRec {
         const this_: LetRec = create(i, LetRec)
         this_.δ = δ
         this_.e = e
         this_.__version()
         return this_
      }
   }

   export class MatchAs extends Expr {
      e: Expr
      σ: Trie.Trie<Expr>
   
      static at (i: ExprId, e: Expr, σ: Trie.Trie<Expr>): MatchAs {
         const this_: MatchAs = create(i, MatchAs)
         this_.e = e
         this_.σ = σ
         this_.__version()
         return this_
      }
   }

   export class OpName extends Expr {
      opName: Lex.OpName
   
      static at (i: ExprId, opName: Lex.OpName): OpName {
         const this_: OpName = create(i, OpName)
         this_.opName = opName
         this_.__version()
         return this_
      }
   }

   // Like a (traditional) function literal wraps an expression, a prim op literal wraps a prim op; however
   // we never bundle such a thing into a closure, but simply unwrap the contained prim op.
   export class PrimOp extends Expr {
      op: Value.PrimOp

      static at (i: ExprId, op: Value.PrimOp): PrimOp {
         const this_: PrimOp = create(i, PrimOp)
         this_.op = op
         this_.__version()
         return this_
      }
   }

   export class Var extends Expr {
      ident: Lex.Var
   
      static at (i: ExprId, ident: Lex.Var): Var {
         const this_: Var = create(i, Var)
         this_.ident = ident
         this_.__version()
         return this_
      }
   }
}

export class Traced<T extends Value.Value = Value.Value> extends VersionedObject<Eval.Evaluand> {
   trace: Trace.Trace | null
   val: T | null

   static at <T extends Value.Value> (k: Eval.Evaluand, trace: Trace.Trace | null, val: T | null): Traced<T> {
      const this_: Traced<T> = create<Eval.Evaluand, Traced<T>>(k, Traced)
      this_.trace = trace
      this_.val = val
      this_.__version()
      return this_
   }
}

export namespace Trie {
   export class TrieId {
      __TrieId (): void {
         // discriminator
      }
   }
   
   // A trie that arises in the raw syntax.
   export class ExprTrieId extends TrieId {
      i: Expr.ExprId
      
      static make (i: Expr.ExprId): ExprTrieId {
         const this_: ExprTrieId = make(ExprTrieId, i)
         this_.i = i
         return this_
      }
   }
   
      // Not abstract, so that I can assert it as a runtime type. Shouldn't T extend JoinSemilattice<T>?
   export class Trie<T> extends VersionedObject<TrieId> implements JoinSemilattice<Trie<T>> {
      join (σ: Trie<T>): Trie<T> {
         return join(this, σ)
      }
   }

   export class Prim<T> extends Trie<T> {
      body: T
   }

   export class ConstInt<T> extends Prim<T> {
      static is<T> (σ: Trie.Trie<T>): σ is ConstInt<T> {
         return σ instanceof ConstInt
      }

      static at <T> (α: TrieId, body: T): ConstInt<T> {
         const this_: ConstInt<T> = create<TrieId, ConstInt<T>>(α, ConstInt)
         this_.body = body
         this_.__version()
         return this_
      }
   }

   export class ConstStr<T> extends Prim<T> {
      static is<T> (σ: Trie.Trie<T>): σ is ConstStr<T> {
         return σ instanceof ConstStr
      }

      static at <T> (α: TrieId, body: T): ConstStr<T> {
         const this_: ConstStr<T> = create<TrieId, ConstStr<T>>(α, ConstStr)
         this_.body = body
         this_.__version()
         return this_
      }
   }

   export class Constr<T> extends Trie<T> {
      cases: Map<string, T>

      static is<T> (σ: Trie.Trie<T>): σ is Constr<T> {
         return σ instanceof Constr
      }

      static at <T> (α: TrieId, cases: Map<string, T>): Constr<T> {
         const this_: Constr<T> = create<TrieId, Constr<T>>(α, Constr)
         this_.cases = cases
         this_.__version()
         return this_
      }
   }

   export class Var<T> extends Trie<T> {
      x: Lex.Var
      body: T

      static is<T> (σ: Trie.Trie<T>): σ is Var<T> {
         return σ instanceof Var
      }

      static at <T> (α: TrieId, x: Lex.Var, body: T): Var<T> {
         const this_: Var<T> = create<TrieId, Var<T>>(α, Var)
         this_.x = x
         this_.body = body
         this_.__version()
         return this_
      }
   }

   export class Fun<T> extends Trie<T> {
      body: T

      static is<T> (σ: Trie.Trie<T>): σ is Fun<T> {
         return σ instanceof Fun
      }

      static at <T> (α: TrieId, body: T): Fun<T> {
         const this_: Fun<T> = create<TrieId, Fun<T>>(α, Fun)
         this_.body = body
         this_.__version()
         return this_
      }
   }

   class JoinTrieId<T> extends TrieId {
      σ: Trie<T>
      τ: Trie<T>

      static make<T> (σ: Trie<T>, τ: Trie<T>): JoinTrieId<T> {
         const this_: JoinTrieId<T> = make(JoinTrieId, σ, τ)
         this_.σ = σ
         this_.τ = τ
         return this_
      }
   }

   export function join<T extends JoinSemilattice<T>> (σ: Trie<T>, τ: Trie<T>): Trie<T> {
      const α: JoinTrieId<T> = JoinTrieId.make(σ, τ)
      if (σ === null) {
         return τ
      } else
      if (τ === null) {
         return σ
      } else
      if (Fun.is(σ) && Fun.is(τ)) {
         return Fun.at(α, σ.body.join(τ.body))
      } else
      if (Var.is(σ) && Var.is(τ) && eq(σ.x, τ.x)) {
         return Var.at(α, σ.x, σ.body.join(τ.body))
      } else
      if (Constr.is(σ) && Constr.is(τ)) {
         return Constr.at<T>(α, unionWith([σ.cases, τ.cases], ms => ms.reduce((x, y) => x.join(y))))
      } else {
         return assert(false, "Undefined join.", σ, τ)
      }
   }
}

export namespace Trace {
   export class Trace extends VersionedObject<Eval.Evaluand> {
      __Trace(): void {
         // discriminator
      }
   }
   
   export class App extends Trace {
      func: Traced
      arg: Traced
      body: Trace

      static at (k: Eval.Evaluand, func: Traced, arg: Traced, body: Trace): App {
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
      t: Trace

      __Let (): void {
         // discriminator
      }

      static at (k: Eval.Evaluand, tu: Traced, t: Trace): Let {
         const this_: Let = create(k, Let)
         this_.tu = tu
         this_.t = t
         this_.__version()
         return this_
      }
   }

   export class LetRec extends Trace {
      δ: Expr.RecDefs
      t: Trace
   
      static at (k: Eval.Evaluand, δ: Expr.RecDefs, t: Trace): LetRec {
         const this_: LetRec = create(k, LetRec)
         this_.δ = δ
         this_.t = t
         this_.__version()
         return this_
      }
   }
   
   // See 0.6.1 release notes.
   export class Match extends Trace {
      tu: Traced
      t: Trace

      __Match (): void {
         // discriminator
      }

      static at (k: Eval.Evaluand, tu: Traced, t: Trace): Match {
         const this_: Match = create(k, Match)
         this_.tu = tu
         this_.t = t
         this_.__version()
         return this_
      }
   }

   export class OpName extends Trace {
      x: Lex.OpName
      t: Trace

      static at (k: Eval.Evaluand, x: Lex.OpName, t: Trace): OpName {
         const this_: OpName = create(k, OpName)
         this_.x = x
         this_.t = t
         this_.__version()
         return this_
      }
   }

   // For primitives there is no body, but we will still show how the argument is consumed.
   export class PrimApp extends Trace {
      op: Traced
      arg: Traced

      static at (k: Eval.Evaluand, op: Traced, arg: Traced): PrimApp {
         const this_: PrimApp = create(k, PrimApp)
         this_.op = op
         this_.arg = arg
         this_.__version()
         return this_
      }
   }

   export class Var extends Trace {
      x: Lex.Var
      t: Trace

      static at (k: Eval.Evaluand, x: Lex.Var, t: Trace): Var {
         const this_: Var = create(k, Var)
         this_.x = x
         this_.t = t
         this_.__version()
         return this_
      }
   }
}
