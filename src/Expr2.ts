import { Lexeme } from "./util/parse/Core2"
import { List } from "./BaseTypes2"
import { FiniteMap } from "./FiniteMap2"
import { UnaryOp } from "./Primitive2"
import { Constr as Constrʹ, _, make } from "./Value2"

export namespace Lex {
   // Probably better to replace the Lexeme subtypes with a discriminated union.
   export class Ctr extends Lexeme {
      str: string = _
   }

   export function ctr (str: string): Ctr {
      return make(Ctr, { str })
   }

   // The name of a primitive operation, such as * or +, where that name is /not/ a standard identifier.
   // Other uses of primitive operations are treated as variables.
   export class OpName extends Lexeme {
      str: string = _
   }

   export function opName (str: string): OpName {
      return make(OpName, { str })
   }

   export class Var extends Lexeme {
      str: string = _
   }

   export function var_ (str: string): Var {
      return make(Var, { str })
   }
}

export namespace Expr {
   export abstract class Expr extends Constrʹ<Expr> {
   }

   export class App extends Expr {
      func: Expr = _
      arg: Expr = _
   }

   export class ConstNum extends Expr {
      val: number = _
   }
   
   export class ConstStr extends Expr {
      val: string = _
   }

   export class Var extends Expr {
      x: Lex.Var = _
   }

   export class Constr extends Expr {
      ctr: Lex.Ctr = _
      args: List<Expr> = _
   }

   export function constr (ctr: Lex.Ctr, args: List<Expr>): Constr {
      return make(Constr, { ctr, args })
   }

   export class Fun extends Expr {
      σ: Trie<Expr> = _
   }

   // A let is simply a match where the trie is a variable trie.
   export class Let extends Expr {
      e: Expr = _
      σ: Trie.Var<Expr> = _
   }

   export class PrimOp extends Expr {
      op: UnaryOp = _
   }

   export function primOp (op: UnaryOp): PrimOp {
      return make(PrimOp, { op })
   }

   export class RecDef extends Constrʹ<RecDef> {
      x: Lex.Var = _
      σ: Trie<Expr> = _
   }
 
   export function recDef (x: Lex.Var, σ: Trie<Expr>): RecDef {
      return make(RecDef, { x, σ })
   }

   export class LetRec extends Expr {
      δ: List<RecDef> = _
      e: Expr = _
   }

   export function letRec (δ: List<RecDef>, e: Expr): LetRec {
      return make(LetRec, { δ, e })
   }

   export class MatchAs extends Expr {
      e: Expr = _
      σ: Trie<Expr> = _
   }

   export class BinaryApp extends Expr {
      e1: Expr = _
      opName: Lex.OpName = _
      e2: Expr = _
   }

   export type Trie<K extends Kont<K>> = Trie.Trie<K>

   export interface Kont<K> {
   }

   export type Args<K extends Kont<K>> = Args.Args<K>

   export namespace Args {
      export abstract class Args<K extends Kont<K>> extends Constrʹ<Args<K>> implements Kont<Args<K>> {
      }

      export class End<K extends Kont<K>> extends Args<K> {
         κ: K = _

         static is<K extends Kont<K>> (Π: Args<K>): Π is End<K> {
            return Π instanceof End
         }
      }

      export class Next<K extends Kont<K>> extends Args<K> {
         σ: Trie<Args<K>> = _

         static is<K extends Kont<K>> (Π: Args<K>): Π is Next<K> {
            return Π instanceof Next
         }
      }
   }

   export namespace Trie {
      export abstract class Trie<K extends Kont<K>> extends Constrʹ<Trie<K>> implements Kont<Trie<K>> {
      }

      export class Constr<K extends Kont<K>> extends Trie<K> {
         cases: FiniteMap<string, Args<K>> = _

         static is<K extends Kont<K>> (σ: Trie<K>): σ is Constr<K> {
            return σ instanceof Constr
         }
      }

      export class Var<K extends Kont<K>> extends Trie<K> {
         x: Lex.Var = _
         κ: K = _

         static is<K extends Kont<K>> (σ: Trie<K>): σ is Var<K> {
            return σ instanceof Var
         }
      }
   }
}

export type Expr = Expr.Expr
