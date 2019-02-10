import { __check, assert } from "./util/Core"
import { JoinSemilattice, eq } from "./util/Ord"
import { ExternalObject, Persistent, PersistentObject, at, make } from "./util/Persistent"
import { Lexeme } from "./util/parse/Core"
import { List } from "./BaseTypes"
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
      __subtag: "Lex.Ctr"
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
      __subtag: "Lex.StringLiteral"
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
      __subtag: "Lex.OpName"
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
      __subtag: "Lex.StringLiteral"
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
      __subtag: "Lex.Var"
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
   // Must be joinable, purely so that joining two expressions will fail.
   export abstract class Expr implements PersistentObject, JoinSemilattice<Expr> {
      __subtag: "Expr.Expr"
      abstract constructor_ (...args: Persistent[]): void // TS requires duplicate def

      join (e: Expr): Expr {
         return assert(false, "Expression join unsupported.")
      }
   }

   export class App extends Expr {
      func: Expr
      arg: Expr

      constructor_ (func: Expr, arg: Expr): void {
         this.func = func
         this.arg = arg
      }

      static at (i: ExternalObject, func: Expr, arg: Expr): App {
         return at(i, App, func, arg)
      }
   }

   export class ConstInt extends Expr {
      val: number

      constructor_ (val: number): void {
         this.val = __check(val, x => !Number.isNaN(x))
      }
   
      static at (i: ExternalObject, val: number): ConstInt {
         return at(i, ConstInt, val)
      }
   }
   
   export class ConstStr extends Expr {
      val: string

      constructor_ (val: string): void {
         this.val = val
      }
   
      static at (i: ExternalObject, val: string): ConstStr {
         return at(i, ConstStr, val)
      }
   }
   
   export class Constr extends Expr {
      ctr: Lex.Ctr
      args: List<Expr>

      constructor_ (ctr: Lex.Ctr, args: List<Expr>): void {
         this.ctr = ctr
         this.args = args
      }
   
      static at (i: ExternalObject, ctr: Lex.Ctr, args: List<Expr>): Constr {
         return at(i, Constr, ctr, args)
      }
   }

   export class Fun extends Expr {
      σ: Trie<Expr>

      constructor_ (σ: Trie<Expr>): void {
         this.σ = σ
      }

      static at (i: ExternalObject, σ: Trie<Expr>): Fun {
         return at(i, Fun, σ)
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

      static at (i: ExternalObject, e: Expr, σ: Trie.Var<Expr>): Let {
         return at(i, Let, e, σ)
      }
   }

   export class PrimOp extends Expr {
      op: UnaryOp

      constructor_ (op: UnaryOp): void {
         this.op = op
      }

      static at (i: ExternalObject, op: UnaryOp): PrimOp {
         return at(i, PrimOp, op)
      }
   }

   export class RecDef implements PersistentObject {
      x: Lex.Var
      e: Expr

      constructor_ (x: Lex.Var, e: Expr): void {
         this.x = x
         this.e = e
      }
   
      static at (α: ExternalObject, x: Lex.Var, e: Expr): RecDef {
         return at(α, RecDef, x, e)
      }
   }

   export class LetRec extends Expr {
      δ: List<RecDef>
      e: Expr

      constructor_ (δ: List<RecDef>, e: Expr): void {
         this.δ = δ
         this.e = e
      }

      static at (i: ExternalObject, δ: List<RecDef>, e: Expr): LetRec {
         return at(i, LetRec, δ, e)
      }
   }

   export class MatchAs extends Expr {
      e: Expr
      σ: Trie<Expr>

      constructor_ (e: Expr, σ: Trie<Expr>): void {
         this.e = e
         this.σ = σ
      }
   
      static at (i: ExternalObject, e: Expr, σ: Trie<Expr>): MatchAs {
         return at(i, MatchAs, e, σ)
      }
   }

   export class PrimApp extends Expr {
      e1: Expr
      opName: Lex.OpName
      e2: Expr

      constructor_ (e1: Expr, opName: Lex.OpName, e2: Expr): void {
         this.e1 = e1
         this.opName = opName
         this.e2 = e2
      }

      static at (i: ExternalObject, e1: Expr, opName: Lex.OpName, e2: Expr): PrimApp {
         return at(i, PrimApp, e1, opName, e2)
      }
   }

   export class Var extends Expr {
      x: Lex.Var

      constructor_ (x: Lex.Var): void {
         this.x = x
      }
   
      static at (i: ExternalObject, x: Lex.Var): Var {
         return at(i, Var, x)
      }
   }

   export type Args<K extends JoinSemilattice<K>> = Args.Args<K>

   export namespace Args {
      // n-ary product.
      export abstract class Args<K extends JoinSemilattice<K>> implements PersistentObject, JoinSemilattice<Args<K>> {
         __subtag: "Expr.Args.Args"

         join (Π: Args<K>): Args<K> {
            return Args.join(this, Π)
         }

         static join<K extends JoinSemilattice<K>> (Π: Args<K>, Πʹ: Args<K>): Args<K> {
            if (Π instanceof End && Πʹ instanceof End) {
               return End.make(Π.κ.join(Πʹ.κ))
            } else
            if (Π instanceof Next && Πʹ instanceof Next) {
               return Next.make(Π.σ.join(Πʹ.σ))
            } else {
               return assert(false, "Undefined join.", Π, Πʹ)
            }
         }
      }

      // Maps zero arguments to κ.
      export class End<K extends JoinSemilattice<K> & Persistent> extends Args<K> {
         κ: K

         constructor_ (
            κ: K
         ) {
            this.κ = κ
         }

         static is<K extends JoinSemilattice<K> & Persistent> (Π: Args<K>): Π is End<K> {
            return Π instanceof End
         }

         static make<K extends JoinSemilattice<K> & Persistent> (κ: K): End<K> {
            return make(End, κ) as End<K>
         }
      }

      // Maps a single argument to another args trie.
      export class Next<K extends JoinSemilattice<K>> extends Args<K> {
         σ: Trie<Args<K>>

         constructor_ (
            σ: Trie<Args<K>>
         ) {
            this.σ = σ
         }

         static is<K extends JoinSemilattice<K>> (Π: Args<K>): Π is Next<K> {
            return Π instanceof Next
         }

         static make<K extends JoinSemilattice<K>> (σ: Trie<Args<K>>): Next<K> {
            return make<Next<K>>(Next, σ)
         }
      }
   }

   // Tries are persistent but not versioned, as per the formalism.
   export type Trie<K extends JoinSemilattice<K>> = Trie.Trie<K>

   // Use "any" because can't define recursive type alias.
   export type Kont = Expr | Args<any> | Trie<any>

   export namespace Trie {
      export abstract class Trie<K extends JoinSemilattice<K>> implements PersistentObject, JoinSemilattice<Trie<K>> {
         __subtag: "Expr.Trie"
         abstract constructor_ (...args: Persistent[]): void // TS requires duplicate def
         
         join (τ: Trie<K>): Trie<K> {
            return Trie.join(this, τ)
         }

         static join<K extends JoinSemilattice<K> & Persistent> (σ: Trie<K>, τ: Trie<K>): Trie<K> {
            if (Fun.is(σ) && Fun.is(τ)) {
               return Fun.make(σ.κ.join(τ.κ))
            } else
            if (Var.is(σ) && Var.is(τ) && eq(σ.x, τ.x)) {
               return Var.make(σ.x, σ.κ.join(τ.κ))
            } else
            if (Constr.is(σ) && Constr.is(τ)) {
               return Constr.make(unionWith(σ.cases, τ.cases, Args.Args.join))
            } else {
               return assert(false, "Undefined join.", this, τ)
            }
         }
      }

      export class Prim<K extends JoinSemilattice<K> & Persistent> extends Trie<K> {
         κ: K
         
         constructor_ (
            κ: K
         ) {
            this.κ = κ
         }
      }

      export class ConstInt<K extends JoinSemilattice<K> & Persistent> extends Prim<K> {
         static is<K extends JoinSemilattice<K> & Persistent> (σ: Trie<K>): σ is ConstInt<K> {
            return σ instanceof ConstInt
         }

         static make<K extends JoinSemilattice<K> & Persistent> (κ: K): ConstInt<K> {
            return make(ConstInt, κ) as ConstInt<K>
         }
      }

      export class ConstStr<K extends JoinSemilattice<K> & Persistent> extends Prim<K> {
         static is<K extends JoinSemilattice<K> & Persistent> (σ: Trie<K>): σ is ConstStr<K> {
            return σ instanceof ConstStr
         }

         static make<K extends JoinSemilattice<K> & Persistent> (κ: K): ConstStr<K> {
            return make(ConstStr, κ) as ConstStr<K>
         }
      }

      // n-ary sum of n-ary products.
      export class Constr<K extends JoinSemilattice<K>> extends Trie<K> {
         cases: FiniteMap<string, Args<K>>

         constructor_ (
            cases: FiniteMap<string, Args<K>>
         ) {
            this.cases = cases
         }

         static is<K extends JoinSemilattice<K>> (σ: Trie<K>): σ is Constr<K> {
            return σ instanceof Constr
         }

         static make<K extends JoinSemilattice<K>> (cases: FiniteMap<string, Args<K>>): Constr<K> {
            return make(Constr, cases)
         }
      }

      export class Fun<K extends JoinSemilattice<K> & Persistent> extends Trie<K> {
         κ: K

         constructor_ (
            κ: K
         ) {
            this.κ = κ
         }

         static is<K extends JoinSemilattice<K> & Persistent> (σ: Trie<K>): σ is Fun<K> {
            return σ instanceof Fun
         }

         static make<K extends JoinSemilattice<K> & Persistent> (κ: K): Fun<K> {
            return make(Fun, κ) as Fun<K>
         }
      }

      export class Var<K extends JoinSemilattice<K> & Persistent> extends Trie<K> {
         x: Lex.Var
         κ: K

         constructor_ (
            x: Lex.Var,
            κ: K
         ) {
            this.x = x
            this.κ = κ
         }

         static is<K extends JoinSemilattice<K> & Persistent> (σ: Trie<K>): σ is Var<K> {
            return σ instanceof Var
         }

         static make<K extends JoinSemilattice<K> & Persistent> (x: Lex.Var, κ: K): Var<K> {
            return make(Var, x, κ) as Var<K>
         }
      }
   }
}
