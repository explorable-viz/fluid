import { __check, absurd, assert } from "./util/Core"
import { eq } from "./util/Ord"
import { Persistent, PersistentObject, at, make } from "./util/Persistent"
import { Lexeme } from "./util/parse/Core"
import { Annotated } from "./Annotated"
import { List, Pair } from "./BaseTypes"
import { FiniteMap, unionWith } from "./FiniteMap"
import { UnaryOp } from "./Primitive"

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
   export class Ctr extends Lexeme {
      __tag: "Lex.Ctr"
      str: string

      constructor_ (
         str: string
      ) {
         this.str = str
      }

      static make (str: string): Ctr {
         return make(Ctr, str)
      }
   }

   // Literal lexemes are elided when constructing abstract syntax to avoid additional level of structure.
   export class IntLiteral extends Lexeme {
      str: string

      constructor_ (
         str: string
      ) {
         this.str = str
      }

      toNumber (): number {
         return parseInt(this.str)
      }

      static make (str: string): IntLiteral {
         return make(IntLiteral, str)
      }
   }

   // Keywords also elided, but we'll probably want that in the syntax at some point.
   export class Keyword extends Lexeme {
      __tag: "Lex.StringLiteral"
      str: string

      constructor_ (
         str: string
      ) {
         this.str = str
      }

      static make (str: string): Keyword {
         return make(Keyword, str)
      }
   }

   // The name of a primitive operation, such as * or +, where that name is /not/ a standard identifier.
   // Other uses of primitive operations are treated as variables.
   export class OpName extends Lexeme {
      __tag: "Lex.OpName"
      str: string

      constructor_ (
         str: string
      ) {
         this.str = str
      }

      static make (str: string): OpName {
         return make(OpName, str)
      }
   }

   export class StringLiteral extends Lexeme {
      __tag: "Lex.StringLiteral"
      str: string

      constructor_ (
         str: string
      ) {
         this.str = str
      }

      toString (): string {
         return str.quotes + this.str + str.quotes
      }

      static make (str: string): StringLiteral {
         return make(StringLiteral, str)
      }
   }

   export class Var extends Lexeme {
      __tag: "Lex.Var"
      str: string

      constructor_ (
         str: string
      ) {
         this.str = str
      }

      static make (str: string): Var {
         return make(Var, str)
      }
   }
}

export type Expr = Expr.Expr

export namespace Expr {
   export abstract class Expr extends Annotated implements PersistentObject {
      __tag: "Expr.Expr"
      abstract constructor_ (...args: Persistent[]): void 
   }

   export class App extends Expr {
      func: Expr
      arg: Expr

      constructor_ (func: Expr, arg: Expr): void {
         this.func = func
         this.arg = arg
      }

      static at (k: PersistentObject, func: Expr, arg: Expr): App {
         return at(k, App, func, arg)
      }
   }

   export class ConstInt extends Expr {
      val: number

      constructor_ (val: number): void {
         this.val = __check(val, x => !Number.isNaN(x))
      }
   
      static at (k: PersistentObject, val: number): ConstInt {
         return at(k, ConstInt, val)
      }
   }
   
   export class ConstStr extends Expr {
      val: string

      constructor_ (val: string): void {
         this.val = val
      }
   
      static at (k: PersistentObject, val: string): ConstStr {
         return at(k, ConstStr, val)
      }
   }
   
   export class Constr extends Expr {
      ctr: Lex.Ctr
      args: List<Expr>

      constructor_ (ctr: Lex.Ctr, args: List<Expr>): void {
         this.ctr = ctr
         this.args = args
      }
   
      static at (k: PersistentObject, ctr: Lex.Ctr, args: List<Expr>): Constr {
         return at(k, Constr, ctr, args)
      }
   }

   export class Fun extends Expr {
      σ: Trie<Expr>

      constructor_ (σ: Trie<Expr>): void {
         this.σ = σ
      }

      static at (k: PersistentObject, σ: Trie<Expr>): Fun {
         return at(k, Fun, σ)
      }
   }

   // A let is simply a match where the trie is a variable trie.
   export class Let extends Expr {
      e: Expr
      σ: Trie.Var<Expr>

      constructor_ (e: Expr, σ: Trie.Var<Expr>): void {
         this.e = e
         this.σ = σ
      }

      static at (k: PersistentObject, e: Expr, σ: Trie.Var<Expr>): Let {
         return at(k, Let, e, σ)
      }
   }

   export class PrimOp extends Expr {
      op: UnaryOp

      constructor_ (op: UnaryOp): void {
         this.op = op
      }

      static at (k: PersistentObject, op: UnaryOp): PrimOp {
         return at(k, PrimOp, op)
      }
   }

   export class RecDef implements PersistentObject {
      x: Lex.Var
      f: Fun

      constructor_ (x: Lex.Var, f: Fun): void {
         this.x = x
         this.f = f
      }
 
      static at (k: PersistentObject, x: Lex.Var, f: Fun): RecDef {
         return at(k, RecDef, x, f)
      }
   }

   export class LetRec extends Expr {
      δ: List<RecDef>
      e: Expr

      constructor_ (δ: List<RecDef>, e: Expr): void {
         this.δ = δ
         this.e = e
      }

      static at (k: PersistentObject, δ: List<RecDef>, e: Expr): LetRec {
         return at(k, LetRec, δ, e)
      }
   }

   export class MatchAs extends Expr {
      e: Expr
      σ: Trie<Expr>

      constructor_ (e: Expr, σ: Trie<Expr>): void {
         this.e = e
         this.σ = σ
      }
   
      static at (k: PersistentObject, e: Expr, σ: Trie<Expr>): MatchAs {
         return at(k, MatchAs, e, σ)
      }
   }

   export class BinaryApp extends Expr {
      e1: Expr
      opName: Lex.OpName
      e2: Expr

      constructor_ (e1: Expr, opName: Lex.OpName, e2: Expr): void {
         this.e1 = e1
         this.opName = opName
         this.e2 = e2
      }

      static at (k: PersistentObject, e1: Expr, opName: Lex.OpName, e2: Expr): BinaryApp {
         return at(k, BinaryApp, e1, opName, e2)
      }
   }

   export class Var extends Expr {
      x: Lex.Var

      constructor_ (x: Lex.Var): void {
         this.x = x
      }
   
      static at (k: PersistentObject, x: Lex.Var): Var {
         return at(k, Var, x)
      }
   }

   export type Args<K extends Kont<K>> = Args.Args<K>

   export namespace Args {
      // n-ary product.
      export abstract class Args<K extends Kont<K>> implements Kont<Args<K>> {
         __tag: "Expr.Args.Args"
         abstract constructor_ (...args: Persistent[]): void

         static join<K extends Kont<K>> (Π: Args<K>, Πʹ: Args<K>): Args<K> {
            if (Π instanceof End && Πʹ instanceof End) {
               return End.make(join(Π.κ, Πʹ.κ))
            } else
            if (Π instanceof Next && Πʹ instanceof Next) {
               return Next.make(join(Π.σ, Πʹ.σ))
            } else {
               return assert(false, "Undefined join.", Π, Πʹ)
            }
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

         static make<K extends Kont<K>> (κ: K): End<K> {
            return make(End, κ) as End<K>
         }
      }

      // Maps a single argument to another args trie.
      export class Next<K extends Kont<K>> extends Args<K> {
         σ: Trie<Args<K>>

         constructor_ (σ: Trie<Args<K>>) {
            this.σ = σ
         }

         static is<K extends Kont<K>> (Π: Args<K>): Π is Next<K> {
            return Π instanceof Next
         }

         static make<K extends Kont<K>> (σ: Trie<Args<K>>): Next<K> {
            return make(Next, σ) as Next<K>
         }
      }
   }

   // Tries are persistent but not versioned, as per the formalism.
   export type Trie<K extends Kont<K>> = Trie.Trie<K>

   export interface Kont<K> extends PersistentObject {
      // bit meaningless if empty
   }

   // Don't understand how polymorphism interacts with subtyping, so brute-force this instead. 
   // Use the same heinous cast as used in 'instantiateKont'.
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

   // Unit continuation.
   export class VoidKont implements Kont<VoidKont> {
      __tag: "Expr.VoidKont"

      constructor_ (): void {
      }

      static make (): VoidKont {
         return make(VoidKont)
      }
   }   

   export namespace Trie {
      export abstract class Trie<K extends Kont<K>> implements Kont<Trie<K>> {
         __tag: "Expr.Trie"
         abstract constructor_ (...args: Persistent[]): void

         static join<K extends Kont<K>> (σ: Trie<K>, τ: Trie<K>): Trie<K> {
            if (Var.is(σ) && Var.is(τ) && eq(σ.x, τ.x)) {
               return Var.make(σ.x, join(σ.κ, τ.κ))
            } else
            if (Constr.is(σ) && Constr.is(τ)) {
               return Constr.make(unionWith(σ.cases, τ.cases, Args.Args.join))
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

         static make<K extends Kont<K>> (cases: FiniteMap<string, Args<K>>): Constr<K> {
            return make(Constr, cases)
         }
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

         static make<K extends Kont<K>> (x: Lex.Var, κ: K): Var<K> {
            return make(Var, x, κ) as Var<K>
         }
      }

      function mapArgs<K extends Kont<K>, Kʹ extends Kont<Kʹ>> (f: (κ: K) => Kʹ): (Π: Args<K>) => Args<Kʹ> {
         return (Π: Args<K>): Args<Kʹ> => {
            if (Args.End.is(Π)) {
               return Args.End.make(f(Π.κ))
            } else
            if (Args.Next.is(Π)) {
               return Args.Next.make(mapTrie(mapArgs(f))(Π.σ))
            } else {
               return absurd()
            }
         }
      }
      
      export function mapTrie<K extends Kont<K>, Kʹ extends Kont<Kʹ>> (f: (κ: K) => Kʹ): (σ: Trie<K>) => Trie<Kʹ> {
         return (σ: Trie<K>): Trie.Trie<Kʹ> => {
            if (Var.is(σ)) {
               return Var.make(σ.x, f(σ.κ))
            } else 
            if (Constr.is(σ)) {
               return Constr.make(σ.cases.map(({ fst: ctr, snd: Π }): Pair<string, Args<Kʹ>> => {
                  if (Π instanceof Args.Args) {
                     return Pair.make(ctr, mapArgs(f)(Π))
                  } else {
                     return absurd()
                  }
               }))
            } else {
               return absurd()
            }
         }
      }
   }
}
