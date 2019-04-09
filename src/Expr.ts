import { Annotation } from "./util/Annotated"
import { __check, absurd, assert } from "./util/Core"
import { eq } from "./util/Ord"
import { Persistent, PersistentObject, make } from "./util/Persistent"
import { AnnotatedVersioned, at } from "./util/Versioned"
import { Lexeme } from "./util/parse/Core"
import { List } from "./BaseTypes"
import { FiniteMap, unionWith } from "./FiniteMap"
import { UnaryOp } from "./Primitive"

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
   export class Ctr extends Lexeme {
      __tag: "Lex.Ctr"
      str: string

      constructor_ (str: string) {
         this.str = str
      }
   }

   export function ctr (str: string): Ctr {
      return make(Ctr, str)
   }

   // Literal lexemes are elided when constructing abstract syntax to avoid additional level of structure.
   export class IntLiteral extends Lexeme {
      __tag: "Lex.IntLiteral"
      str: string

      constructor_ (str: string) {
         this.str = str
      }

      toNumber (): number {
         return parseInt(this.str)
      }
   }

   export function intLiteral (str: string): IntLiteral {
      return make(IntLiteral, str)
   }

   export class NumLiteral extends Lexeme {
      __tag: "Lex.NumLiteral"
      str: string

      constructor_ (str: string) {
         this.str = str
      }

      toNumber (): number {
         return new Number(this.str).valueOf()
      }
   }

   export function numLiteral (str: string): NumLiteral {
      return make(NumLiteral, str)
   }

   // Keywords also elided, but we'll probably want that in the syntax at some point.
   export class Keyword extends Lexeme {
      __tag: "Lex.Keyword"
      str: string

      constructor_ (str: string) {
         this.str = str
      }
   }

   export function keyword (str: string): Keyword {
      return make(Keyword, str)
   }

   // The name of a primitive operation, such as * or +, where that name is /not/ a standard identifier.
   // Other uses of primitive operations are treated as variables.
   export class OpName extends Lexeme {
      __tag: "Lex.OpName"
      str: string

      constructor_ (str: string) {
         this.str = str
      }
   }

   export function opName (str: string): OpName {
      return make(OpName, str)
   }

   export class StringLiteral extends Lexeme {
      __tag: "Lex.StringLiteral"
      str: string

      constructor_ (str: string) {
         this.str = str
      }

      toString (): string {
         return str.quotes + this.str + str.quotes
      }
   }

   export function strLiteral (str: string): StringLiteral {
      return make(StringLiteral, str)
   }

   export class Var extends Lexeme {
      __tag: "Lex.Var"
      str: string

      constructor_ (str: string) {
         this.str = str
      }
   }

   export function var_ (str: string): Var {
      return make(Var, str)
   }
}

export type Expr = Expr.Expr
export type Kont<K> = Expr.Kont<K>

export namespace Expr {
   export abstract class Expr extends AnnotatedVersioned implements Kont<Expr> {
      __tag: "Expr.Expr"
      abstract constructor_ (...args: Persistent[]): void 
   }

   export class App extends Expr {
      func: Expr
      arg: Expr

      constructor_ (α: Annotation, func: Expr, arg: Expr): void {
         this.α = α
         this.func = func
         this.arg = arg
      }
   }

   export function app (k: PersistentObject, α: Annotation, func: Expr, arg: Expr): App {
      return at(k, App, α, func, arg)
   }

   export class ConstInt extends Expr {
      val: number

      constructor_ (α: Annotation, val: number): void {
         this.α = α
         this.val = __check(val, x => !Number.isNaN(x))
      }
   }
   
   export function constInt (k: PersistentObject, α: Annotation, val: number): ConstInt {
      return at(k, ConstInt, α, val)
   }

   export class ConstNum extends Expr {
      val: number

      constructor_ (α: Annotation, val: number): void {
         this.α = α
         this.val = __check(val, x => !Number.isNaN(x))
      }
   }
   
   export function constNum (k: PersistentObject, α: Annotation, val: number): ConstNum {
      return at(k, ConstNum, α, val)
   }

   export class ConstStr extends Expr {
      val: string

      constructor_ (α: Annotation, val: string): void {
         this.α = α
         this.val = val
      }
   }
   
   export function constStr (k: PersistentObject, α: Annotation, val: string): ConstStr {
      return at(k, ConstStr, α, val)
   }

   export class Constr extends Expr {
      ctr: Lex.Ctr
      args: List<Expr>

      constructor_ (α: Annotation, ctr: Lex.Ctr, args: List<Expr>): void {
         this.α = α
         this.ctr = ctr
         this.args = args
      }
   }
   
   export function constr (k: PersistentObject, α: Annotation, ctr: Lex.Ctr, args: List<Expr>): Constr {
      return at(k, Constr, α, ctr, args)
   }

   export class Fun extends Expr {
      σ: Trie<Expr>

      constructor_ (α: Annotation, σ: Trie<Expr>): void {
         this.α = α
         this.σ = σ
      }
   }

   export function fun (k: PersistentObject, α: Annotation, σ: Trie<Expr>): Fun {
      return at(k, Fun, α, σ)
   }

   // A let is simply a match where the trie is a variable trie.
   export class Let extends Expr {
      e: Expr
      σ: Trie.Var<Expr>

      constructor_ (α: Annotation, e: Expr, σ: Trie.Var<Expr>): void {
         this.α = α
         this.e = e
         this.σ = σ
      }
   }

   export function let_ (k: PersistentObject, α: Annotation, e: Expr, σ: Trie.Var<Expr>): Let {
      return at(k, Let, α, e, σ)
   }

   export class PrimOp extends Expr {
      op: UnaryOp

      constructor_ (α: Annotation, op: UnaryOp): void {
         this.α = α
         this.op = op
      }
   }

   export function primOp (k: PersistentObject, α: Annotation, op: UnaryOp): PrimOp {
      return at(k, PrimOp, α, op)
   }

   export class RecDef extends AnnotatedVersioned {
      x: Lex.Var
      σ: Trie<Expr>

      constructor_ (α: Annotation, x: Lex.Var, σ: Trie<Expr>): void {
         this.α = α
         this.x = x
         this.σ = σ
      }
   }
 
   export function recDef (k: PersistentObject, α: Annotation, x: Lex.Var, σ: Trie<Expr>): RecDef {
      return at(k, RecDef, α, x, σ)
   }

   export class LetRec extends Expr {
      δ: List<RecDef>
      e: Expr

      constructor_ (α: Annotation, δ: List<RecDef>, e: Expr): void {
         this.α = α
         this.δ = δ
         this.e = e
      }
   }

   export function letRec (k: PersistentObject, α: Annotation, δ: List<RecDef>, e: Expr): LetRec {
      return at(k, LetRec, α, δ, e)
   }

   export class MatchAs extends Expr {
      e: Expr
      σ: Trie<Expr>

      constructor_ (α: Annotation, e: Expr, σ: Trie<Expr>): void {
         this.α = α
         this.e = e
         this.σ = σ
      }
   }
   
   export function matchAs (k: PersistentObject, α: Annotation, e: Expr, σ: Trie<Expr>): MatchAs {
      return at(k, MatchAs, α, e, σ)
   }

   export class BinaryApp extends Expr {
      e1: Expr
      opName: Lex.OpName
      e2: Expr

      constructor_ (α: Annotation, e1: Expr, opName: Lex.OpName, e2: Expr): void {
         this.α = α
         this.e1 = e1
         this.opName = opName
         this.e2 = e2
      }
   }

   export function binaryApp (k: PersistentObject, α: Annotation, e1: Expr, opName: Lex.OpName, e2: Expr): BinaryApp {
      return at(k, BinaryApp, α, e1, opName, e2)
   }

   export class Var extends Expr {
      x: Lex.Var

      constructor_ (α: Annotation, x: Lex.Var): void {
         this.α = α
         this.x = x
      }
   }
   
   export function var_ (k: PersistentObject, α: Annotation, x: Lex.Var): Var {
      return at(k, Var, α, x)
   }

   export type Args<K extends Kont<K>> = Args.Args<K>

   export namespace Args {
      // n-ary product.
      export abstract class Args<K extends Kont<K>> implements Kont<Args<K>> {
         __tag: "Expr.Args.Args"
         abstract constructor_ (...args: Persistent[]): void

         static join<K extends Kont<K>> (Π: Args<K>, Πʹ: Args<K>): Args<K> {
            if (Π instanceof End && Πʹ instanceof End) {
               return end(join(Π.κ, Πʹ.κ))
            } else
            if (Π instanceof Next && Πʹ instanceof Next) {
               return next(join(Π.σ, Πʹ.σ))
            } else {
               return assert(false, "Undefined join.", Π, Πʹ)
            }
         }

         static is<K extends Kont<K>> (Π: Object): Π is Args<K> {
            return Π instanceof Args
         }
      }

      // Maps zero arguments to κ.
      export class End<K extends Kont<K>> extends Args<K> {
         κ: K

         constructor_ (κ: K) {
            this.κ = κ
         }

         static is<K extends Kont<K>> (Π: Args<K>): Π is End<K> {
            return Π instanceof End
         }
      }

      export function end<K extends Kont<K>> (κ: K): End<K> {
         return make(End, κ) as End<K>
      }

      // Maps a single argument to another args trie.
      export class Next<K extends Kont<K>> extends Args<K> {
         σ: Trie<Args<K>>

         constructor_ (σ: Trie<Args<K>>): void {
            this.σ = σ
         }

         static is<K extends Kont<K>> (Π: Args<K>): Π is Next<K> {
            return Π instanceof Next
         }
      }

      export function next<K extends Kont<K>> (σ: Trie<Args<K>>): Next<K> {
         return make(Next, σ) as Next<K>
      }
   }

   // Tries are now versioned, since (trie id, value id) is needed to determine match id.
   export type Trie<K extends Kont<K>> = Trie.Trie<K>

   export interface Kont<K> extends PersistentObject {
      __tag: string // require continuation types to be pseudo-nominal
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

   // Tries are interned, not versioned, as per the formalism; it might make sense for tries to be versioned so we 
   // can key match ids on (value id, trie id), but resist for now to avoid having to synthesise ids for the 
   // output of join, instantiate and mapTrie.
   export namespace Trie {
      export abstract class Trie<K extends Kont<K>> implements Kont<Trie<K>> {
         __tag: "Expr.Trie"
         abstract constructor_ (...args: Persistent[]): void

         static join<K extends Kont<K>> (σ: Trie<K>, τ: Trie<K>): Trie<K> {
            if (Var.is(σ) && Var.is(τ) && eq(σ.x, τ.x)) {
               return var_(σ.x, join(σ.κ, τ.κ))
            } else
            if (Constr.is(σ) && Constr.is(τ)) {
               return constr(unionWith(σ.cases, τ.cases, Args.Args.join))
            } else {
               return assert(false, "Undefined join.", this, τ)
            }
         }
      }

      // n-ary sum of n-ary products.
      export class Constr<K extends Kont<K>> extends Trie<K> {
         cases: FiniteMap<string, Args<K>>

         constructor_ (cases: FiniteMap<string, Args<K>>) {
            this.cases = cases
         }

         static is<K extends Kont<K>> (σ: Trie<K>): σ is Constr<K> {
            return σ instanceof Constr
         }
      }

      export function constr<K extends Kont<K>> (cases: FiniteMap<string, Args<K>>): Constr<K> {
         return make(Constr, cases)
      }

      export class Var<K extends Kont<K>> extends Trie<K> {
         x: Lex.Var
         κ: K

         constructor_ (x: Lex.Var, κ: K) {
            this.x = x
            this.κ = κ
         }

         static is<K extends Kont<K>> (σ: Trie<K>): σ is Var<K> {
            return σ instanceof Var
         }
      }

      export function var_<K extends Kont<K>> (x: Lex.Var, κ: K): Var<K> {
         return make(Var, x, κ) as Var<K>
      }
   }
}
