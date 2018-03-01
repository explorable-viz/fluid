import { __nonNull, as, asOpt, assert } from "./util/Core"
import { unionWith } from "./util/Map"
import { JoinSemilattice, eq } from "./util/Ord"
import { Lexeme } from "./util/parse/Core"
import { __def, key } from "./Memo"
import { partiallyApply } from "./Primitive"
import { binaryOps, create, projections, unaryOps } from "./Runtime"

export class EnvEntry {
   ρ: Env
   e: Expr.Expr

   constructor(ρ: Env, e: Expr.Expr) {
      this.ρ = ρ
      this.e = e  
   }
}

export type Env = Map<string, EnvEntry | null>

export namespace str {
   // Primitive ops.
   export const concat: string = '++'
   export const div: string = '/'
   export const equal: string = '=='
   export const greaterT: string = '>'
   export const lessT: string = '<'
   export const minus: string = '-'
   export const plus: string = '+'
   export const times: string = '*'

   // Constants used for parsing, and also for toString() implementations.
   export const arrow: string = '→'
   export const as: string = "as"
   export const equals: string = '='
   export const fun: string = "fun"
   export const in_: string = "in"
   export const let_: string = "let"
   export const letRec: string = "letrec"
   export const match: string = "match"
   export const parenL: string = '('
   export const parenR: string = ')'
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

export type Value = Closure | ConstInt | ConstStr | Constr | PrimOp

// Primitive ops; see 0.4.4 release notes.
export class PrimOp {
   _apply (v: Value | null): Value | null {
      if (v === null) {
         return null
      } else {
         return this.__apply(v)
      }
   }

   __apply (v: Value): Value {
      return assert(false, "Would like this to be abstract.")
   }
}

// Assume all dynamic type-checking is performed inside the underlying JS operation, although
// currently there mostly isn't any.
export class UnaryPrimOp extends PrimOp {
   name: string

   static at (α: Addr, name: string): UnaryPrimOp {
      const this_: UnaryPrimOp = create(α, UnaryPrimOp)
      this_.name = name
      this_.__version()
      return this_
   }

   __apply (v: Value): Value {
      return __nonNull(unaryOps.get(this.name))(v)
   }

   toString (): string {
      return this.name
   }
}

export class BinaryPrimOp extends PrimOp {
   name: string

   static at (α: Addr, name: string): BinaryPrimOp {
      const this_: BinaryPrimOp = create(α, BinaryPrimOp)
      this_.name = name
      this_.__version()
      return this_
   }

   __apply (v1: Value): PrimOp {
      return partiallyApply(this, v1)
   }

   
   toString (): string {
      return this.name
   }
}

// Binary op that has been applied to a single operand.
export class UnaryPartialPrimOp extends PrimOp {
   name: string
   binOp: BinaryPrimOp
   v1: Value

   static at (α: Addr, name: string, binOp: BinaryPrimOp, v1: Value): UnaryPartialPrimOp {
      const this_: UnaryPartialPrimOp = create(α, UnaryPartialPrimOp)
      this_.name = name
      this_.binOp = as(binOp, BinaryPrimOp)
      this_.v1 = v1
      this_.__version()
      return this_
   }

   __apply (v2: Value): Value {
      return __nonNull(binaryOps.get(this.binOp.name))(this.v1, v2)
   }

   toString (): string {
      return this.name
   }
}

// Syntactically distinguish projection functions from other unary ops, previously because we generated an
// implementation; may no longer be necessary.
export class Proj extends PrimOp {
   name: string

   static at (α: Addr, name: string): Proj {
      const this_: Proj = create(α, Proj)
      this_.name = name
      this_.__version()
      return this_
   }

   __apply (v: Value): Value {
      return __nonNull(projections.get(this.name))(v)
   }

   toString (): string {
      return this.name
   }
}

export class Closure {
   ρ: Env
   defs: RecDefinition[]
   func: Fun

   static at (α: Addr, ρ: Env, defs: RecDefinition[], func: Fun): Closure {
      const this_: Closure = create(α, Closure)
      this_.ρ = ρ
      this_.defs = defs
      this_.func = as(func, Fun)
      this_.__version()
      return this_
   }
}

export class ConstInt {
   val: number

   static at (α: Addr, val: number): ConstInt {
      const this_: ConstInt = create(α, ConstInt)
      this_.val = val
      this_.__version()
      return this_
   }
}

export class ConstStr {
   val: string

   static at (α: Addr, val: string): ConstStr {
      const this_: ConstStr = create(α, ConstStr)
      this_.val = val
      this_.__version()
      return this_
   }
}

export class Constr {
   ctr: Lex.Ctr
   args: Traced[]

   static at (α: Addr, ctr: Lex.Ctr, args: Traced[]): Constr {
      const this_: Constr = create(α, Constr)
      this_.ctr = as(ctr, Lex.Ctr)
      this_.args = args
      this_.__version()
      return this_
   }
}

export class Traced<T extends Value = Value> {
   trace: Trace
   val: T | null

   static at <T extends Value> (α: Addr, trace: Trace, val: T | null): Traced<T> {
      const this_: Traced<T> = create<Traced<T>>(α, Traced)
      this_.trace = as(trace, Trace)
      this_.val = val
      this_.__version()
      return this_
   }
}

export class Trace {
}

// I don't think this is the same as ⊥; it represents the "end" of an explanation.
export class EmptyTrace extends Trace {
   static at (α: Addr): EmptyTrace {
      const this_: Trace = create(α, EmptyTrace)
      this_.__version()
      return this_
   }
}

export class OpName extends Trace {
   opName: Lex.OpName

   static at (α: Addr, opName: Lex.OpName): OpName {
      const this_: OpName = create(α, OpName)
      this_.opName = as(opName, Lex.OpName)
      this_.__version()
      return this_
   }
}

// Expression form only. TODO: don't I need to unify this now with Closure?
export class Fun extends Trace {
   σ: Trie<Traced>

   static at (α: Addr, σ: Trie<Traced>): Fun {
      const this_: Fun = create(α, Fun)
      this_.σ = as(σ, Trie)
      this_.__version()
      return this_
   }
}

// Not abstract, so that I can assert it as a runtime type. Shouldn't T extend JoinSemilattice<T>?
export class Trie<T> implements JoinSemilattice<Trie<T>> {
   join (σ: Trie<T>): Trie<T> {
      return join(this, σ)
   }
}

export class ConstrTrie<T> extends Trie<T> {
   cases: Map<string, T>

   static at <T> (α: Addr, cases: Map<string, T>): ConstrTrie<T> {
      const this_: ConstrTrie<T> = create<ConstrTrie<T>>(α, ConstrTrie)
      this_.cases = cases
      this_.__version()
      return this_
   }
}

export class VarTrie<T> extends Trie<T> {
   name: Lex.Var
   body: T

   static at <T> (α: Addr, name: Lex.Var, body: T): VarTrie<T> {
      const this_: VarTrie<T> = create<VarTrie<T>>(α, VarTrie)
      this_.name = as(name, Lex.Var)
      this_.body = body
      this_.__version()
      return this_
   }
}

export class FunTrie<T> extends Trie<T> {
   body: T

   static at <T> (α: Addr, body: T): FunTrie<T> {
      const this_: FunTrie<T> = create<FunTrie<T>>(α, FunTrie)
      this_.body = body
      this_.__version()
      return this_
   }
}

export class RecDefinition {
   name: Lex.Var
   func: Fun

   static at (α: Addr, name: Lex.Var, func: Fun): RecDefinition {
      const this_: RecDefinition = create(α, RecDefinition)
      this_.name = as(name, Lex.Var)
      this_.func = as(func, Fun)
      this_.__version()
      return this_
   }
}

// Keep binding of recursive definitions to closures separate from the definitions themselves so that
// closures can contain definitions without inducing cycles.
export class RecBinding {
   def: RecDefinition
   valueOpt: Closure | null

   static at (α: Addr, def: RecDefinition, valueOpt: Closure | null): RecBinding {
      const this_: RecBinding = create(α, RecBinding)
      this_.def = as(def, RecDefinition)
      this_.valueOpt = asOpt(valueOpt, Closure)
      this_.__version()
      return this_
   }
}

export class LetRec extends Trace {
   bindings: RecBinding[]
   body: Trace

   static at (α: Addr, bindings: RecBinding[], body: Trace): LetRec {
      const this_: LetRec = create(α, LetRec)
      this_.bindings = bindings
      this_.body = as(body, Trace)
      this_.__version()
      return this_
   }
}

export namespace Trace {
   export class App extends Trace {
      func: Traced
      arg: Traced
      body: Trace

      static at (α: Addr, func: Traced, arg: Traced, body: Trace): App {
         const this_: App = create(α, App)
         this_.func = as(func, Traced)
         this_.arg = as(arg, Traced)
         this_.body = as(body, Trace)
         this_.__version()
         return this_
      }
   }

   // For primitives there is no body, but we will still show how the argument is consumed.
   export class PrimApp extends Trace {
      func: Traced
      arg: Traced

      static at (α: Addr, func: Traced, arg: Traced): App {
         const this_: App = create(α, App)
         this_.func = as(func, Traced)
         this_.arg = as(arg, Traced)
         this_.__version()
         return this_
      }
   }

   export class Let extends Trace {
      tu: Traced
      t: Trace

      static at (α: Addr, tu: Traced, t: Trace): Match {
         const this_: Match = create(α, Match)
         this_.tu = as(tu, Traced)
         this_.t = as(t, Trace)
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
         this_.tu = as(tu, Traced)
         this_.t = as(t, Trace)
         this_.__version()
         return this_
      }
   }

   export class Var extends Trace {
      x: Lex.Var
      t: Trace

      static at (α: Addr, x: Lex.Var, t: Trace): Var {
         const this_: Var = create(α, Var)
         this_.x = as(x, Lex.Var)
         this_.t = as(t, Trace)
         this_.__version()
         return this_
      }
   }
}

export namespace Expr {
   export class Expr {
   }

   export class App extends Expr {
      func: Expr
      arg: Expr

      static at (α: Addr, func: Expr, arg: Expr): App {
         const this_: App = create(α, App)
         this_.func = as(func, Expr)
         this_.arg = as(arg, Expr)
         this_.__version()
         return this_
      }
   }

   // A let is simply a match where the trie is a variable trie.
   export class Let extends Expr {
      e: Expr
      σ: VarTrie<Expr>

      static at (α: Addr, e: Expr, σ: VarTrie<Expr>): Let {
         const this_: Let = create(α, Let)
         this_.e = as(e, Expr)
         this_.σ = as(σ, VarTrie)
         this_.__version()
         return this_
      }
   }

   export class MatchAs extends Expr {
      e: Expr
      σ: Trie<Expr>
   
      static at (α: Addr, e: Traced, σ: Trie<Expr>): MatchAs {
         const this_: MatchAs = create(α, MatchAs)
         this_.e = as(e, Traced)
         this_.σ = as(σ, Trie)
         this_.__version()
         return this_
      }
   }

   export class Var extends Expr {
      ident: Lex.Var
   
      static at (α: Addr, ident: Lex.Var): Var {
         const this_: Var = create(α, Var)
         this_.ident = as(ident, Lex.Var)
         this_.__version()
         return this_
      }
   }
}

// Addressing scheme doesn't yet support "member functions". Plus methods don't allow null receivers.
__def(join)
export function join <T extends JoinSemilattice<T>> (σ: Trie<T>, τ: Trie<T>): Trie<T> {
   const α: Addr = key(join, arguments)
   if (σ === null) {
      return τ
   } else
   if (τ === null) {
      return σ
   } else
   // The instanceof guards turns T into 'any'. Yuk.
   if (σ instanceof FunTrie && τ instanceof FunTrie) {
      const [σʹ, τʹ]: [FunTrie<T>, FunTrie<T>] = [σ, τ]
      return FunTrie.at(α, join(σʹ.body, τʹ.body))
   } else
   if (σ instanceof VarTrie && τ instanceof VarTrie && eq(σ.name, τ.name)) {
      const [σʹ, τʹ]: [VarTrie<T>, VarTrie<T>] = [σ, τ]
      return VarTrie.at(α, σʹ.name, join(σʹ.body, τʹ.body))
   } else
   if (σ instanceof ConstrTrie && τ instanceof ConstrTrie) {
      const [σʹ, τʹ]: [ConstrTrie<T>, ConstrTrie<T>] = [σ, τ]
      return ConstrTrie.at<T>(α, unionWith([σʹ.cases, τʹ.cases], ms => ms.reduce((x, y) => x.join(y))))
   } else {
      return assert(false, "Undefined join.", σ, τ)
   }
}
