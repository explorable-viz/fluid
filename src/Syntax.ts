import { __check, assert, make } from "./util/Core"
import { unionWith } from "./util/Map"
import { JoinSemilattice, eq } from "./util/Ord"
import { Lexeme } from "./util/parse/Core"
import { Env, EnvId } from "./Env"
import { PrimBody } from "./Primitive"
import { Id, PersistentObject, RawId, create } from "./Runtime"

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
   export class ValId extends Id {
      __ValId(): void {
         // discriminator
      }
   }

   export class Value extends PersistentObject<ValId> {
      __Value(): void {
         // discriminator
      }
   }

   export class Closure extends Value {
      ρ: Env
      j: EnvId
      δ: Expr.RecDefs
      func: Expr.Fun
   
      static at (α: ValId, ρ: Env, j: EnvId, δ: Expr.RecDefs, func: Expr.Fun): Closure {
         const this_: Closure = create(α, Closure)
         this_.ρ = ρ
         this_.j = j
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
   export class ExprId extends Id {
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
      
   export class Expr extends PersistentObject<ExprId> {
      __Expr(): void {
         // discriminator
      }
   }

   export class App extends Expr {
      func: Expr
      arg: Expr

      static at (α: ExprId, func: Expr, arg: Expr): App {
         const this_: App = create(α, App)
         this_.func = func
         this_.arg = arg
         this_.__version()
         return this_
      }
   }

   export class ConstInt extends Expr {
      val: number
   
      static at (α: ExprId, val: number): ConstInt {
         const this_: ConstInt = create(α, ConstInt)
         this_.val = __check(val, x => !Number.isNaN(x))
         this_.__version()
         return this_
      }
   }
   
   export class ConstStr extends Expr {
      val: string
   
      static at (α: ExprId, val: string): ConstStr {
         const this_: ConstStr = create(α, ConstStr)
         this_.val = val
         this_.__version()
         return this_
      }
   }
   
   export class Constr extends Expr {
      ctr: Lex.Ctr
      args: Expr[]
   
      static at (α: ExprId, ctr: Lex.Ctr, args: Expr[]): Constr {
         const this_: Constr = create(α, Constr)
         this_.ctr = ctr
         this_.args = args
         this_.__version()
         return this_
      }
   }

   export class Fun extends Expr {
      σ: Trie.Trie<Expr>

      static at (α: ExprId, σ: Trie.Trie<Expr>): Fun {
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

      static at (α: Id, e: Expr, σ: Trie.Var<Expr>): Let {
         const this_: Let = create(α, Let)
         this_.e = e
         this_.σ = σ
         this_.__version()
         return this_
      }
   }

   export class RecDefId extends Id {
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

   export class RecDef extends PersistentObject<RecDefId> {
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

   export class RecDefsId extends Id {
      i: ExprId

      __RecDefsId(): void {
         // discriminator
      }
   
      static make (i: ExprId): RecDefsId {
         const this_: RecDefsId = make(RecDefsId, i)
         this_.i = i
         return this_
      }
   }

   export class RecDefs extends PersistentObject<RecDefsId> {
      defs: RecDef[]
   
      static at (α: RecDefsId, defs: RecDef[]): RecDefs {
         const this_: RecDefs = create(α, RecDefs)
         this_.defs = defs
         this_.__version()
         return this_
      }
   }

   export class LetRec extends Expr {
      δ: RecDefs
      e: Expr

      static at (α: ExprId, δ: RecDefs, e: Expr): LetRec {
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
   
      static at (α: Id, e: Expr, σ: Trie.Trie<Expr>): MatchAs {
         const this_: MatchAs = create(α, MatchAs)
         this_.e = e
         this_.σ = σ
         this_.__version()
         return this_
      }
   }

   export class OpName extends Expr {
      opName: Lex.OpName
   
      static at (α: Id, opName: Lex.OpName): OpName {
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

      static at (α: Id, op: Value.PrimOp): PrimOp {
         const this_: PrimOp = create(α, PrimOp)
         this_.op = op
         this_.__version()
         return this_
      }
   }

   export class Var extends Expr {
      ident: Lex.Var
   
      static at (α: ExprId, ident: Lex.Var): Var {
         const this_: Var = create(α, Var)
         this_.ident = ident
         this_.__version()
         return this_
      }
   }
}

export class TracedId extends Id {
   __TracedId (): void {
      // discriminator
   }
}

export class Traced<T extends Value.Value = Value.Value> extends PersistentObject<TracedId> {
   trace: Trace.Trace
   val: T | null

   static at <T extends Value.Value> (α: TracedId, trace: Trace.Trace, val: T | null): Traced<T> {
      const this_: Traced<T> = create<TracedId, Traced<T>>(α, Traced)
      this_.trace = trace
      this_.val = val
      this_.__version()
      return this_
   }
}

export namespace Trie {
   export class TrieId extends Id {
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
   export class Trie<T> extends PersistentObject<TrieId> implements JoinSemilattice<Trie<T>> {
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

   class JoinTrieId extends TrieId {
      σ_id: TrieId
      τ_id: TrieId

      static make (σ_id: TrieId, τ_id: TrieId): JoinTrieId {
         const this_: JoinTrieId = make(JoinTrieId, σ_id, τ_id)
         this_.σ_id = σ_id
         this_.τ_id = τ_id
         return this_
      }
   }

   export function join<T extends JoinSemilattice<T>> (σ: Trie<T>, τ: Trie<T>): Trie<T> {
      const α: JoinTrieId = JoinTrieId.make(σ.__id, τ.__id)
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

export class TraceId extends Id {
   __TraceId(): void {
      // discriminator
   }
}

export namespace Trace {
   export class Trace extends PersistentObject<TraceId> {
      __Trace(): void {
         // discriminator
      }
   }
   
   export class App extends Trace {
      func: Traced
      arg: Traced
      body: Trace

      static at (α: TraceId, func: Traced, arg: Traced, body: Trace): App {
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
      static at (α: TraceId): Empty {
         const this_: Empty = create(α, Empty)
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

      static at (α: TraceId, tu: Traced, t: Trace): Let {
         const this_: Let = create(α, Let)
         this_.tu = tu
         this_.t = t
         this_.__version()
         return this_
      }
   }

   // Used to be something called RecBinding, but bindings doesn't seem to be stored in traces at the moment.
   export class LetRec extends Trace {
      δ: Expr.RecDefs
      t: Trace
   
      static at (α: TraceId, δ: Expr.RecDefs, t: Trace): LetRec {
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

      __Match (): void {
         // discriminator
      }

      static at (α: TraceId, tu: Traced, t: Trace): Match {
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

      static at (α: TraceId, x: Lex.OpName, t: Trace): OpName {
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

      static at (α: TraceId, op: Traced, arg: Traced): PrimApp {
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

      static at (α: TraceId, x: Lex.Var, t: Trace): Var {
         const this_: Var = create(α, Var)
         this_.x = x
         this_.t = t
         this_.__version()
         return this_
      }
   }
}
