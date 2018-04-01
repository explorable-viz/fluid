import { assert } from "./util/Core"
import { unionWith } from "./util/Map"
import { JoinSemilattice, eq } from "./util/Ord"
import { Lexeme } from "./util/parse/Core"
import { Env } from "./Env"
import { key, Addr, PersistentObject } from "./Memo"
import { PrimBody } from "./Primitive"
import { create } from "./Runtime"

export namespace str {
   // Primitive ops.
   export const concat: string = "++"
   export const div: string = "/"
   export const equal: string = "=="
   export const greaterT: string = ">"
   export const lessT: string = "<"
   export const minus: string = "-"
   export const plus: string = "+"
   export const times: string = "*"

   // Constants used for parsing, and also for toString() implementations.
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
   export class Value extends PersistentObject {
      __Value(): void {
         // discriminator
      }
   }

   export class Closure extends Value {
      ρ: Env
      δ: Expr.RecDefinition[]
      func: Expr.Fun
   
      static at (α: Addr, ρ: Env, δ: Expr.RecDefinition[], func: Expr.Fun): Closure {
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
   
      static at (α: Addr, val: number): ConstInt {
         const this_: ConstInt = create(α, ConstInt)
         this_.val = val
         this_.__version()
         return this_
      }
   }
   
   export class ConstStr extends Prim {
      val: string
   
      static at (α: Addr, val: string): ConstStr {
         const this_: ConstStr = create(α, ConstStr)
         this_.val = val
         this_.__version()
         return this_
      }
   }
   
   export class Constr extends Value {
      ctr: Lex.Ctr
      args: Traced[]
   
      static at (α: Addr, ctr: Lex.Ctr, args: Traced[]): Constr {
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

      static at (α: Addr, name: string, σ: Trie.Prim<PrimBody<any>>): PrimOp {
         const this_: PrimOp = create(α, PrimOp)
         this_.name = name
         this_.σ = σ
         this_.__version()
         return this_
      }
   }
}

export namespace Expr {
   export class Expr extends PersistentObject {
      __Expr(): void {
         // discriminator
      }
   }

   export class App extends Expr {
      func: Expr
      arg: Expr

      static at (α: Addr, func: Expr, arg: Expr): App {
         const this_: App = create(α, App)
         this_.func = func
         this_.arg = arg
         this_.__version()
         return this_
      }
   }

   export class ConstInt extends Expr {
      val: number
   
      static at (α: Addr, val: number): ConstInt {
         const this_: ConstInt = create(α, ConstInt)
         this_.val = val
         this_.__version()
         return this_
      }
   }
   
   export class ConstStr extends Expr {
      val: string
   
      static at (α: Addr, val: string): ConstStr {
         const this_: ConstStr = create(α, ConstStr)
         this_.val = val
         this_.__version()
         return this_
      }
   }
   
   export class Constr extends Expr {
      ctr: Lex.Ctr
      args: Expr[]
   
      static at (α: Addr, ctr: Lex.Ctr, args: Expr[]): Constr {
         const this_: Constr = create(α, Constr)
         this_.ctr = ctr
         this_.args = args
         this_.__version()
         return this_
      }
   }

   export class Fun extends Expr {
      σ: Trie.Trie<Expr>

      static at (α: Addr, σ: Trie.Trie<Expr>): Fun {
         const this_: Fun = create(α, Fun)
         this_.σ = σ
         this_.__version()
         return this_
      }
   }

   // A let is simply a match where the trie is a variable trie.
   export class Let extends Expr {
      e: Expr
      σ: Trie.Var<Expr>

      static at (α: Addr, e: Expr, σ: Trie.Var<Expr>): Let {
         const this_: Let = create(α, Let)
         this_.e = e
         this_.σ = σ
         this_.__version()
         return this_
      }
   }

   export class RecDefinition extends PersistentObject {
      name: Lex.Var
      func: Fun
   
      static at (α: Addr, name: Lex.Var, func: Fun): RecDefinition {
         const this_: RecDefinition = create(α, RecDefinition)
         this_.name = name
         this_.func = func
         this_.__version()
         return this_
      }
   }
   
   export class LetRec extends Expr {
      δ: RecDefinition[]
      e: Expr

      static at (α: Addr, δ: RecDefinition[], e: Expr): LetRec {
         const this_: LetRec = create(α, LetRec)
         this_.δ = δ
         this_.e = e
         this_.__version()
         return this_
      }
   }

   export class MatchAs extends Expr {
      e: Expr
      σ: Trie.Trie<Expr>
   
      static at (α: Addr, e: Expr, σ: Trie.Trie<Expr>): MatchAs {
         const this_: MatchAs = create(α, MatchAs)
         this_.e = e
         this_.σ = σ
         this_.__version()
         return this_
      }
   }

   export class OpName extends Expr {
      opName: Lex.OpName
   
      static at (α: Addr, opName: Lex.OpName): OpName {
         const this_: OpName = create(α, OpName)
         this_.opName = opName
         this_.__version()
         return this_
      }
   }

   // Like a (traditional) function literal wraps an expression, a prim op literal wraps a prim op; however
   // we never bundle such a thing into a closure, but simply unwrap the contained prim op.
   export class PrimOp extends Expr {
      op: Value.PrimOp

      static at (α: Addr, op: Value.PrimOp): PrimOp {
         const this_: PrimOp = create(α, PrimOp)
         this_.op = op
         this_.__version()
         return this_
      }
   }

   export class Var extends Expr {
      ident: Lex.Var
   
      static at (α: Addr, ident: Lex.Var): Var {
         const this_: Var = create(α, Var)
         this_.ident = ident
         this_.__version()
         return this_
      }
   }
}

export class Traced<T extends Value.Value = Value.Value> extends PersistentObject {
   trace: Trace.Trace
   val: T | null

   static at <T extends Value.Value> (α: Addr, trace: Trace.Trace, val: T | null): Traced<T> {
      const this_: Traced<T> = create<Traced<T>>(α, Traced)
      this_.trace = trace
      this_.val = val
      this_.__version()
      return this_
   }
}

export namespace Trie {
   // Not abstract, so that I can assert it as a runtime type. Shouldn't T extend JoinSemilattice<T>?
   export class Trie<T> extends PersistentObject implements JoinSemilattice<Trie<T>> {
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

      static at <T> (α: Addr, body: T): ConstInt<T> {
         const this_: ConstInt<T> = create<ConstInt<T>>(α, ConstInt)
         this_.body = body
         this_.__version()
         return this_
      }
   }

   export class ConstStr<T> extends Prim<T> {
      static is<T> (σ: Trie.Trie<T>): σ is ConstStr<T> {
         return σ instanceof ConstStr
      }

      static at <T> (α: Addr, body: T): ConstStr<T> {
         const this_: ConstStr<T> = create<ConstStr<T>>(α, ConstStr)
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

      static at <T> (α: Addr, cases: Map<string, T>): Constr<T> {
         const this_: Constr<T> = create<Constr<T>>(α, Constr)
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

      static at <T> (α: Addr, x: Lex.Var, body: T): Var<T> {
         const this_: Var<T> = create<Var<T>>(α, Var)
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

      static at <T> (α: Addr, body: T): Fun<T> {
         const this_: Fun<T> = create<Fun<T>>(α, Fun)
         this_.body = body
         this_.__version()
         return this_
      }
   }

   export function join<T extends JoinSemilattice<T>> (σ: Trie<T>, τ: Trie<T>): Trie<T> {
      const α: Addr = key(join, arguments)
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
   export class Trace extends PersistentObject {
      __Trace(): void {
         // discriminator
      }
   }
   
   export class App extends Trace {
      func: Traced
      arg: Traced
      body: Trace

      static at (α: Addr, func: Traced, arg: Traced, body: Trace): App {
         const this_: App = create(α, App)
         this_.func = func
         this_.arg = arg
         this_.body = body
         this_.__version()
         return this_
      }
   }

   // I don't think this is the same as ⊥; it represents the "end" of an explanation.
   export class Empty extends Trace {
      static at (α: Addr): Empty {
         const this_: Empty = create(α, Empty)
         this_.__version()
         return this_
      }
   }

   export class Let extends Trace {
      tu: Traced
      t: Trace

      static at (α: Addr, tu: Traced, t: Trace): Match {
         const this_: Match = create(α, Match)
         this_.tu = tu
         this_.t = t
         this_.__version()
         return this_
      }
   }

   // Used to be something called RecBinding, but bindings doesn't seem to be stored in traces at the moment.
   export class LetRec extends Trace {
      δ: Expr.RecDefinition[]
      t: Trace
   
      static at (α: Addr, δ: Expr.RecDefinition[], t: Trace): LetRec {
         const this_: LetRec = create(α, LetRec)
         this_.δ = δ
         this_.t = t
         this_.__version()
         return this_
      }
   }
   
      // See 0.6.1 release notes. Also 0.6.4 notes for discussion of expression/trace disparity.
   export class Match extends Trace {
      tu: Traced
      t: Trace

      static at (α: Addr, tu: Traced, t: Trace): Match {
         const this_: Match = create(α, Match)
         this_.tu = tu
         this_.t = t
         this_.__version()
         return this_
      }
   }

   export class OpName extends Trace {
      x: Lex.OpName
      t: Trace

      static at (α: Addr, x: Lex.OpName, t: Trace): OpName {
         const this_: OpName = create(α, OpName)
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

      static at (α: Addr, op: Traced, arg: Traced): PrimApp {
         const this_: PrimApp = create(α, PrimApp)
         this_.op = op
         this_.arg = arg
         this_.__version()
         return this_
      }
   }

   export class Var extends Trace {
      x: Lex.Var
      t: Trace

      static at (α: Addr, x: Lex.Var, t: Trace): Var {
         const this_: Var = create(α, Var)
         this_.x = x
         this_.t = t
         this_.__version()
         return this_
      }
   }
}
