import { as } from "./util/Core"
import { Lexeme } from "./util/parse/Core"
import { Str } from "./BaseTypes"
import { create, Traced } from "./Runtime"

// Constants used for parsing, and also for toString() implementations.
export namespace str {
   export const quotes: string = '"'
   export const as: string = "as"
   export const match: string = "match"
   export const fun: string = "fun"
   export const in_: string = "in"
   export const let_: string = "let"
   export const letRec: string = "letrec"
}

export namespace Lex {
   // With purely structural typing, these lexeme classes are identical, not just isomorphic. This
   // mostly sucks in a class-oriented languages like JavaScript, so we add dummy discriminator methods.

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

export class Trace {
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
