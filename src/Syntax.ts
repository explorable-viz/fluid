import { Lexeme } from "./util/parse/Core"
import { create, typeCheck_ } from "./Runtime"

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
   _param: ITraced<Str>

   static at (α: Addr, param: ITraced<Str>): PrimBody {
      const this_: PrimBody = create(α, PrimBody)
      this_._param = typeCheck_(param, Str)
      this_.__version()
      return this_
   }

   static at_ (α: Addr, param: Str): PrimBody {
      return PrimBody.at(α, __val(keyP(α, 'param'), param))
   }

   get param (): Str {
      return this._param.val
   }
}

export class FunBody extends AppBody {
   _σ: ITraced<Trie<ITraced>>

   static at (α: Addr, σ: ITraced<Trie<ITraced>>): FunBody {
      const this_: FunBody = create(α, FunBody)
      this_._σ = typeCheck(σ, ITraced)
      this_.__version()
      return this_
   }

   static at_ (α: Addr, σ: Trie<ITraced>): FunBody {
      return FunBody.at(α, __val(keyP(α, 'σ'), σ))
   }

   get σ (): Trie<ITraced> {
      return this._σ.val
   }
}
