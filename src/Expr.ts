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
      σ: Trie

      static at (i: ExternalObject, σ: Trie): Fun {
         const this_: Fun = create(i, Fun)
         this_.σ = σ
         this_.__version()
         return this_
      }
   }

   // A let is simply a match where the trie is a variable trie.
   export class Let extends Expr {
      e: Expr
      σ: Trie.Var

      static at (i: ExternalObject, e: Expr, σ: Trie.Var): Let {
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
      σ: Trie
   
      static at (i: ExternalObject, e: Expr, σ: Trie): MatchAs {
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

   // Distinguish tries as an expression form; using a single parameterised definition is messy.
   export type Kont = Expr | Trie | Trie.Args

   // Tries are persistent but not versioned, as per the formalism.
   export type Trie = Trie.Trie

   export namespace Trie {
      export class Trie extends PersistentObject implements JoinSemilattice<Trie> {
         join (σ: Trie): Trie {
            return join(this, σ)
         }
      }

      export class Prim extends Trie {
         κ: Kont
      }

      export class ConstInt extends Prim {
         static make (κ: Kont): ConstInt {
            const this_: ConstInt = make(ConstInt, κ)
            this_.κ = κ
            return this_
         }
      }

      export class ConstStr extends Prim {
         static make (κ: Kont): ConstStr {
            const this_: ConstStr = make(ConstStr, κ)
            this_.κ = κ
            return this_
         }
      }

      // n-ary product.
      export class Args extends PersistentObject {
         __Trie_Args (): void {
            // discriminator
         }
      }

      // Maps zero arguments to κ.
      export class Nil extends Args {
         κ: Kont

         static make (κ: Kont): Nil {
            const this_: Nil = make(Nil, κ)
            this_.κ = κ
            return this_
         }
      }

      // Maps a single argument to another args trie.
      export class Cons extends Args {
         σ: Trie

         static make (σ: Trie): Cons {
            const this_: Cons = make(Cons, σ)
            this_.σ = σ
            return this_
         }
      }

      // n-ary sum of n-ary products.
      export class Constr extends Trie {
         cases: FiniteMap<string, Args>

         static make (cases: FiniteMap<string, Args>): Constr {
            const this_: Constr = make(Constr, cases)
            this_.cases = cases
            return this_
         }
      }

      export class Fun extends Trie {
         κ: Kont

         static make (κ: Kont): Fun {
            const this_: Fun = make(Fun, κ)
            this_.κ = κ
            return this_
         }
      }

      export class Var extends Trie {
         x: Lex.Var
         κ: Kont

         static make (x: Lex.Var, κ: Kont): Var {
            const this_: Var = make(Var, x, κ)
            this_.x = x
            this_.κ = κ
            return this_
         }
      }

      // join of expressions is undefined, which effectively means case branches never overlap.
      function joinKont (κ: Kont, κʹ: Kont): Kont {
         if (κ instanceof Trie && κʹ instanceof Trie) {
            return join(κ, κʹ)
         } else
         if (κ instanceof Args && κʹ instanceof Args) {
            return joinArgs(κ, κʹ)
         } else {
            return assert(false, "Undefined join.", κ, κʹ)
         }
      }

      function joinArgs (Π: Args, Πʹ: Args): Args {
         if (Π instanceof Nil && Πʹ instanceof Nil) {
            return Nil.make(joinKont(Π.κ, Πʹ.κ))
         } else
         if (Π instanceof Cons && Πʹ instanceof Cons) {
            return Cons.make(join(Π.σ, Πʹ.σ))
         } else {
            return assert(false, "Undefined join.", Π, Πʹ)
         }
      }

      // Want to give this a polymorphic type, but doesn't work properly with instanceof.
      export function join (σ: Trie, τ: Trie): Trie {
         if (σ instanceof Fun && τ instanceof Fun) {
            return Fun.make(joinKont(σ.κ, τ.κ))
         } else
         if (σ instanceof Var && τ instanceof Var && eq(σ.x, τ.x)) {
            return Var.make(σ.x, joinKont(σ.κ, τ.κ))
         } else
         if (σ instanceof Constr && τ instanceof Constr) {
            return Constr.make(unionWith(σ.cases, τ.cases, joinArgs))
         } else {
            return assert(false, "Undefined join.", σ, τ)
         }
      }
   }
}
