import { __check, assert, make } from "./util/Core"
import { JoinSemilattice, eq } from "./util/Ord"
import { Lexeme } from "./util/parse/Core"
import { List } from "./BaseTypes"
import { FiniteMap, unionWith } from "./FiniteMap"
import { UnaryOp } from "./Primitive"
import { ExternalObject, PersistentObject, VersionedObject, create } from "./Runtime"

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
      constructor(str: string) {
         super(str)
      }

      __Lex_Ctr(): void {
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

      __Lex_OpName(): void {
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

      __Lex_Var(): void {
         // discriminator
      }
   }
}

export type Expr = Expr.Expr

export namespace Expr {
   export class Expr extends VersionedObject<ExternalObject> {
      __Expr_Expr(): void {
         // discriminator
      }
   }

   export class App extends Expr {
      func: Expr
      arg: Expr

      static at (i: ExternalObject, func: Expr, arg: Expr): App {
         const this_: App = create(i, App)
         this_.func = func
         this_.arg = arg
         this_.__version()
         return this_
      }
   }

   export class ConstInt extends Expr {
      val: number
   
      static at (i: ExternalObject, val: number): ConstInt {
         const this_: ConstInt = create(i, ConstInt)
         this_.val = __check(val, x => !Number.isNaN(x))
         this_.__version()
         return this_
      }
   }
   
   export class ConstStr extends Expr {
      val: string
   
      static at (i: ExternalObject, val: string): ConstStr {
         const this_: ConstStr = create(i, ConstStr)
         this_.val = val
         this_.__version()
         return this_
      }
   }
   
   export class Constr extends Expr {
      ctr: Lex.Ctr
      args: List<Expr>
   
      static at (i: ExternalObject, ctr: Lex.Ctr, args: List<Expr>): Constr {
         const this_: Constr = create(i, Constr)
         this_.ctr = ctr
         this_.args = args
         this_.__version()
         return this_
      }
   }

   export class Fun extends Expr {
      σ: Trie<Expr>

      static at (i: ExternalObject, σ: Trie<Expr>): Fun {
         const this_: Fun = create(i, Fun)
         this_.σ = σ
         this_.__version()
         return this_
      }
   }

   // A let is simply a match where the trie is a variable trie.
   export class Let extends Expr {
      e: Expr
      σ: Trie.Var<Expr>

      static at (i: ExternalObject, e: Expr, σ: Trie.Var<Expr>): Let {
         const this_: Let = create(i, Let)
         this_.e = e
         this_.σ = σ
         this_.__version()
         return this_
      }
   }

   export class PrimOp extends Expr {
      op: UnaryOp

      static at (i: ExternalObject, op: UnaryOp): PrimOp {
         const this_: PrimOp = create(i, PrimOp)
         this_.op = op
         this_.__version()
         return this_
      }
   }

   export class RecDef extends VersionedObject<ExternalObject> {
      x: Lex.Var
      e: Expr
   
      static at (α: ExternalObject, x: Lex.Var, e: Expr): RecDef {
         const this_: RecDef = create(α, RecDef)
         this_.x = x
         this_.e = e
         this_.__version()
         return this_
      }
   }

   export class LetRec extends Expr {
      δ: List<RecDef>
      e: Expr

      static at (i: ExternalObject, δ: List<RecDef>, e: Expr): LetRec {
         const this_: LetRec = create(i, LetRec)
         this_.δ = δ
         this_.e = e
         this_.__version()
         return this_
      }
   }

   export class MatchAs extends Expr {
      e: Expr
      σ: Trie<Expr>
   
      static at (i: ExternalObject, e: Expr, σ: Trie<Expr>): MatchAs {
         const this_: MatchAs = create(i, MatchAs)
         this_.e = e
         this_.σ = σ
         this_.__version()
         return this_
      }
   }

   export class PrimApp extends Expr {
      e1: Expr
      opName: Lex.OpName
      e2: Expr

      static at (i: ExternalObject, e1: Expr, opName: Lex.OpName, e2: Expr): PrimApp {
         const this_: PrimApp = create(i, PrimApp)
         this_.e1 = e1
         this_.opName = opName
         this_.e2 = e2
         this_.__version()
         return this_
      }
   }

   export class Var extends Expr {
      x: Lex.Var
   
      static at (i: ExternalObject, x: Lex.Var): Var {
         const this_: Var = create(i, Var)
         this_.x = x
         this_.__version()
         return this_
      }
   }

   // Tries are persistent but not versioned, as per the formalism.
   export type Trie<K> = Trie.Trie<K>

   export namespace Trie {
      export class Trie<K> extends PersistentObject {
         static join<K extends JoinSemilattice<K>> (σ: Trie<K>, τ: Trie<K>): Trie<K> {
            if (σ instanceof Fun && τ instanceof Fun) {
               return Fun.make(σ.κ.join(τ.κ))
            } else
            if (σ instanceof Var && τ instanceof Var && eq(σ.x, τ.x)) {
               return Var.make(σ.x, σ.κ.join(τ.κ))
            } else
            if (σ instanceof Constr && τ instanceof Constr) {
               return Constr.make(unionWith(σ.cases, τ.cases, Args.join))
            } else {
               return assert(false, "Undefined join.", this, τ)
            }
         }
      }

      export class Prim<K> extends Trie<K> {
         κ: K
      }

      export class ConstInt<K> extends Prim<K> {
         static make<K> (κ: K): ConstInt<K> {
            const this_: ConstInt<K> = make<ConstInt<K>>(ConstInt, κ)
            this_.κ = κ
            return this_
         }
      }

      export class ConstStr<K> extends Prim<K> {
         static make<K> (κ: K): ConstStr<K> {
            const this_: ConstStr<K> = make<ConstStr<K>>(ConstStr, κ)
            this_.κ = κ
            return this_
         }
      }

      // n-ary product.
      export class Args<K> extends PersistentObject {
         __Trie_Args (): void {
            // discriminator
         }

         static join<K extends JoinSemilattice<K>> (Π: Args<K>, Πʹ: Args<K>): Args<K> {
            if (Π instanceof End && Πʹ instanceof End) {
               return End.make(Π.κ.join(Πʹ.κ))
            } else
            if (Π instanceof Next && Πʹ instanceof Next) {
               return Next.make(Trie.join(Π.σ, Πʹ.σ))
            } else {
               return assert(false, "Undefined join.", Π, Πʹ)
            }
         }
      }

      // Maps zero arguments to κ.
      export class End<K> extends Args<K> {
         κ: K

         static make<K> (κ: K): End<K> {
            const this_: End<K> = make<End<K>>(End, κ)
            this_.κ = κ
            return this_
         }
      }

      // Maps a single argument to another args trie.
      export class Next<K> extends Args<K> {
         σ: Trie<Args<K>>

         static make<K> (σ: Trie<Args<K>>): Next<K> {
            const this_: Next<K> = make<Next<K>>(Next, σ)
            this_.σ = σ
            return this_
         }
      }

      // n-ary sum of n-ary products.
      export class Constr<K> extends Trie<K> {
         cases: FiniteMap<string, Args<K>>

         static make<K> (cases: FiniteMap<string, Args<K>>): Constr<K> {
            const this_: Constr<K> = make(Constr, cases)
            this_.cases = cases
            return this_
         }
      }

      export class Fun<K> extends Trie<K> {
         κ: K

         static make<K> (κ: K): Fun<K> {
            const this_: Fun<K> = make<Fun<K>>(Fun, κ)
            this_.κ = κ
            return this_
         }
      }

      export class Var<K> extends Trie<K> {
         x: Lex.Var
         κ: K

         static make<K> (x: Lex.Var, κ: K): Var<K> {
            const this_: Var<K> = make<Var<K>>(Var, x, κ)
            this_.x = x
            this_.κ = κ
            return this_
         }
      }
   }
}
