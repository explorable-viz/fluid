import { as, assert } from "./util/Core"
import { JoinSemilattice } from "./util/Ord"
import { Lexeme } from "./util/parse/Core"
import { Str } from "./BaseTypes"
import { create, Traced } from "./Runtime"

// Constants used for parsing, and also for toString() implementations.
export namespace str {
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

export type Value = ConstInt | ConstStr | Constr

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

export class Var extends Trace {
   ident: Lex.Var

   static at (α: Addr, ident: Lex.Var): Var {
      const this_: Var = create(α, Var)
      this_.ident = as(ident, Lex.Var)
      this_.__version()
      return this_
   }
}

// Expression form only. TODO: don't I need to unify this now with Closure?
export class Fun extends Trace {
   x: Lex.Var
   e: Traced

   static at (α: Addr, x: Lex.Var, e: Traced): Fun {
      const this_: Fun = create(α, Fun)
      this_.x = as(x, Lex.Var)
      this_.e = as(e, Traced)
      this_.__version()
      return this_
   }
}

// Body of a lambda abstraction or primitive.
export class AppBody {
}

// An application expression has an empty body.
export class EmptyBody extends AppBody {
   static at (α: Addr): EmptyBody {
      return create(α, EmptyBody)
   }
}

// For primitives there is no trace part, but we will still show how the argument is consumed.
// TODO: unify with matches?
export class PrimBody extends AppBody {
   param: Str

   static at (α: Addr, param: Str): PrimBody {
      const this_: PrimBody = create(α, PrimBody)
      this_.param = as(param, Str)
      this_.__version()
      return this_
   }
}

export class FunBody extends AppBody {
   x: Lex.Var
   e: Traced

   static at (α: Addr, x: Lex.Var, e: Traced): FunBody {
      const this_: FunBody = create(α, FunBody)
      this_.x = as(x, Lex.Var)
      this_.e = as(e, Traced)
      this_.__version()
      return this_
   }
}

export class App extends Trace {
   func: Traced
   arg: Traced
   appBody: AppBody

   static at (α: Addr, func: Traced, arg: Traced, appBody: AppBody): App {
      const this_: App = create(α, App)
      this_.func = as(func, Traced)
      this_.arg = as(arg, Traced)
      this_.appBody = as(appBody, AppBody)
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
      const this_: ConstrTrie<T> = create(α, ConstrTrie)
      this_.cases = cases
      this_.__version()
      return this_
   }
}

export class VarTrie<T> extends Trie<T> {
   name: Lex.Var
   body: T

   static at <T> (α: Addr, name: Lex.Var, body: T): VarTrie<T> {
      const this_: VarTrie<T> = create(α, VarTrie)
      this_.name = as(name, Lex.Var)
      this_.body = body
      this_.__version()
      return this_
   }
}

export class FunTrie<T> extends Trie<T> {
   body: T

   static at <T> (α: Addr, body: T): FunTrie<T> {
      const this_: FunTrie<T> = create(α, FunTrie)
      this_.body = body
      this_.__version()
      return this_
   }
}


export function join <T extends JoinSemilattice<T>> (σ: Trie<T>, τ: Trie<T>): Trie<T> {
   return assert(false)
}
