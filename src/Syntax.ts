import { Lexeme } from "./util/parse/Core"

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
