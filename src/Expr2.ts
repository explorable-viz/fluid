import { absurd } from "./util/Core"
import { eq } from "./util/Ord"
import { Lexeme } from "./util/parse/Core2"
import { List } from "./BaseTypes2"
import { FiniteMap, unionWith } from "./FiniteMap2"
import { UnaryOp } from "./Primitive2"
import { Constr as Constrʹ, _, make } from "./Value2"

// Constants used for parsing, and also for toString() implementations.
export namespace str {
   export const arrow: string = "→"
   export const as: string = "as"
   export const bracketL: string = "["
   export const bracketR: string = "]"
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
   // Probably better to replace the Lexeme subtypes with a discriminated union.
   export class Ctr extends Lexeme {
      str: string = _
   }

   export function ctr (str: string): Ctr {
      return make(Ctr, str)
   }

   // Literal lexemes are elided when constructing abstract syntax to avoid additional level of structure.
   export class NumLiteral extends Lexeme {
      str: string = _

      toNumber (): number {
         return new Number(this.str).valueOf()
      }
   }

   export function numLiteral (str: string): NumLiteral {
      return make(NumLiteral, str)
   }

   // Keywords also elided, but we'll probably want that in the syntax at some point.
   export class Keyword extends Lexeme {
      str: string = _
   }

   export function keyword (str: string): Keyword {
      return make(Keyword, str)
   }

   // The name of a primitive operation, such as * or +, where that name is /not/ a standard identifier.
   // Other uses of primitive operations are treated as variables.
   export class OpName extends Lexeme {
      str: string = _
   }

   export function opName (str: string): OpName {
      return make(OpName, str)
   }

   export class StringLiteral extends Lexeme {
      str: string = _

      toString (): string {
         return str.quotes + this.str + str.quotes
      }
   }

   export function strLiteral (str: string): StringLiteral {
      return make(StringLiteral, str)
   }

   // Variable lexemes are also elided, as per literals.
   export class Var extends Lexeme {
      str: string = _
   }

   export function var_ (str: string): Var {
      return make(Var, str)
   }
}

export type Expr = Expr.Expr
export type Kont<K> = Expr.Kont<K>

export namespace Expr {
   export abstract class Expr extends Constrʹ<Expr> {
   }

   export class App extends Expr {
      func: Expr = _
      arg: Expr = _
   }

   export function app (func: Expr, arg: Expr): App {
      return make(App, func, arg)
   }

   export class ConstNum extends Expr {
      val: number = _
   }
   
   export function constNum (val: number): ConstNum {
      return make(ConstNum, val)
   }

   export class ConstStr extends Expr {
      val: string = _
   }

   export function constStr (val: string): ConstStr {
      return make(ConstStr, val)
   }

   export class Constr extends Expr {
      ctr: Lex.Ctr = _
      args: List<Expr> = _
   }

   export function constr (ctr: Lex.Ctr, args: List<Expr>): Constr {
      return make(Constr, ctr, args)
   }

   export class Fun extends Expr {
      σ: Trie<Expr> = _
   }

   export function fun (σ: Trie<Expr>): Fun {
      return make(Fun, σ)
   }

   // A let is simply a match where the trie is a variable trie.
   export class Let extends Expr {
      e: Expr = _
      σ: Trie.Var<Expr> = _
   }

   export function let_ (e: Expr, σ: Trie.Var<Expr>): Let {
      return make(Let, e, σ)
   }

   export class PrimOp extends Expr {
      op: UnaryOp = _
   }

   export function primOp (op: UnaryOp): PrimOp {
      return make(PrimOp, op)
   }

   export class RecDef extends Constrʹ<RecDef> {
      x: Lex.Var = _
      σ: Trie<Expr> = _
   }
 
   export function recDef (x: Lex.Var, σ: Trie<Expr>): RecDef {
      return make(RecDef, x, σ)
   }

   export class LetRec extends Expr {
      δ: List<RecDef> = _
      e: Expr = _
   }

   export function letRec (δ: List<RecDef>, e: Expr): LetRec {
      return make(LetRec, δ, e)
   }

   export class MatchAs extends Expr {
      e: Expr = _
      σ: Trie<Expr> = _
   }

   export function matchAs (e: Expr, σ: Trie<Expr>): MatchAs {
      return make(MatchAs, e, σ)
   }

   export class BinaryApp extends Expr {
      e1: Expr = _
      opName: Lex.OpName = _
      e2: Expr = _
   }

   export function binaryApp (e1: Expr, opName: Lex.OpName, e2: Expr): BinaryApp {
      return make(BinaryApp, e1, opName, e2)
   }

   export class Var extends Expr {
      x: string = _
   }

   export function var_ (x: string): Var {
      return make(Var, x)
   }

   export type Trie<K extends Kont<K>> = Trie.Trie<K>

   export interface Kont<K> extends Constrʹ<K> {
   }

   // Don't understand how polymorphism interacts with subtyping, so brute-force this instead. 
   // Use the same heinous cast as used in 'instantiateKont'. Note this join is unrelated to the annotation lattice.
   function join<K extends Kont<K>> (κ: K, κʹ: K): K {
      if (κ instanceof Trie.Trie && κʹ instanceof Trie.Trie) {
         return Trie.Trie.join<K>(κ, κʹ) as any as K
      } else
      if (κ instanceof Args.Args && κʹ instanceof Args.Args) {
         return Args.Args.join<K>(κ, κʹ) as any as K
      } else {
         return absurd("Unsupported join.")
      }
   }

   export type Args<K extends Kont<K>> = Args.Args<K>

   export namespace Args {
      export abstract class Args<K extends Kont<K>> extends Constrʹ<Args<K>> implements Kont<Args<K>> {
         static join<K extends Kont<K>> (Π: Args<K>, Πʹ: Args<K>): Args<K> {
            if (Π instanceof End && Πʹ instanceof End) {
               return end(join(Π.κ, Πʹ.κ))
            } else
            if (Π instanceof Next && Πʹ instanceof Next) {
               return next(join(Π.σ, Πʹ.σ))
            } else {
               return absurd("Undefined join.", Π, Πʹ)
            }
         }
      }

      export class End<K extends Kont<K>> extends Args<K> {
         κ: K = _

         static is<K extends Kont<K>> (Π: Args<K>): Π is End<K> {
            return Π instanceof End
         }
      }

      export function end<K extends Kont<K>> (κ: K): End<K> {
         return make<End<K>>(End, κ)
      }

      export class Next<K extends Kont<K>> extends Args<K> {
         σ: Trie<Args<K>> = _

         static is<K extends Kont<K>> (Π: Args<K>): Π is Next<K> {
            return Π instanceof Next
         }
      }

      export function next<K extends Kont<K>> (σ: Trie<Args<K>>): Next<K> {
         return make<Next<K>>(Next, σ)
      }
   }

   export namespace Trie {
      export abstract class Trie<K extends Kont<K>> extends Constrʹ<Trie<K>> implements Kont<Trie<K>> {
         static join<K extends Kont<K>> (σ: Trie<K>, τ: Trie<K>): Trie<K> {
            if (Var.is(σ) && Var.is(τ) && eq(σ.x, τ.x)) {
               return var_(σ.x, join(σ.κ, τ.κ))
            } else
            if (Constr.is(σ) && Constr.is(τ)) {
               return constr(unionWith(σ.cases, τ.cases, Args.Args.join))
            } else {
               return absurd("Undefined join.", this, τ)
            }
         }
      }

      export class Constr<K extends Kont<K>> extends Trie<K> {
         cases: FiniteMap<string, Args<K>> = _

         static is<K extends Kont<K>> (σ: Trie<K>): σ is Constr<K> {
            return σ instanceof Constr
         }
      }

      export function constr<K extends Kont<K>> (cases: FiniteMap<string, Args<K>>): Constr<K> {
         return make(Constr, cases)
      }

      export class Var<K extends Kont<K>> extends Trie<K> {
         x: string = _
         κ: K = _

         static is<K extends Kont<K>> (σ: Trie<K>): σ is Var<K> {
            return σ instanceof Var
         }
      }

      export function var_<K extends Kont<K>> (x: string, κ: K): Var<K> {
         return make<Var<K>>(Var, x, κ)
      }
   }
}
