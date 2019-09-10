import { absurd, error } from "./util/Core"
import { eq } from "./util/Ord"
import { Annotated, AnnotatedC } from "./Annotated"
import { List } from "./BaseTypes"
import { ctrToDataType } from "./DataType"
import { DataValue } from "./DataValue"
import { FiniteMap, unionWith } from "./FiniteMap"
import { Id, Num, Str, _, make } from "./Value"
import { ν, at } from "./Versioned"

// Constants used for parsing, and also for toString() implementations.
export namespace strings {
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
   export const primitive: string = "primitive"
   export const parenL: string = "("
   export const parenR: string = ")"
   export const quotes: string = '"'
   export const typematch: string = "typematch"
}

export type Expr = Expr.Expr
export type Cont = Expr.Cont

export namespace Expr {
   // Use to be a parameterised class but we can simplify using our nominal type idiom.
   export type Cont = Expr | DataValue<"Trie">

   // Unrelated to the annotation lattice. Expr case intentionally only defined for higher-order (function) case.
   function join<K extends Cont> (κ: K, κʹ: K): K {
      if (κ instanceof Trie.Trie && κʹ instanceof Trie.Trie) {
         return Trie.Trie.join<K>(κ, κʹ) as K
      } else
      if (κ instanceof Fun && κʹ instanceof Fun) {
         return fun(ν(), join(κ.σ, κʹ.σ)) as Expr as K
      } else {
         return absurd("Undefined join.", κ, κʹ)
      }
   }

   export abstract class Expr extends AnnotatedC(DataValue)<"Expr"> {
   }
   
   export class App extends Expr {
      f: Expr = _
      e: Expr = _
   }

   export function app (k: Id, f: Expr, e: Expr): App {
      return at(k, App, f, e)
   }

   export class BinaryApp extends Expr {
      e1: Expr = _
      opName: Str = _
      e2: Expr = _
   }

   export function binaryApp (k: Id, e1: Expr, opName: Str, e2: Expr): BinaryApp {
      return at(k, BinaryApp, e1, opName, e2)
   }

   export class ConstNum extends Expr {
      val: Num = _
   }
   
   export function constNum (k: Id, val: Num): ConstNum {
      return at(k, ConstNum, val)
   }

   export class ConstStr extends Expr {
      val: Str = _
   }

   export function constStr (k: Id, val: Str): ConstStr {
      return at(k, ConstStr, val)
   }

   export class Constr extends Expr {
      ctr: Str = _
      args: List<Expr> = _
   }

   export function constr (k: Id, ctr: Str, args: List<Expr>): Constr {
      return at(k, Constr, ctr, args)
   }

   export class Def extends AnnotatedC(DataValue)<"Expr.Def"> {
   }

   export class Let extends Def {
      x: Annotated<Str> = _
      e: Expr = _
   }

   export function let_ (k: Id, x: Annotated<Str>, e: Expr): Let {
      return at(k, Let, x, e)
   }

   export class Prim extends Def {
      x: Annotated<Str> = _
   }

   export function prim (k: Id, x: Annotated<Str>): Prim {
      return at(k, Prim, x)
   }

   export class RecDef extends DataValue<"RecDef"> {
      x: Annotated<Str> = _
      σ: Trie<Expr> = _
   }
 
   export function recDef (x: Annotated<Str>, σ: Trie<Expr>): RecDef {
      return make(RecDef, x, σ)
   }

   export class LetRec extends Def {
      δ: List<RecDef> = _
   }

   export function letRec (k: Id, δ: List<RecDef>): LetRec {
      return at(k, LetRec, δ)
   }

   export class Defs extends Expr {
      def̅: List<Def> = _
      e: Expr = _
   }

   export function defs (k: Id, def̅: List<Def>, e: Expr): Defs {
      return at(k, Defs, def̅, e)
   }

   export class Fun extends Expr {
      σ: Trie<Expr> = _
   }

   export function fun (k: Id, σ: Trie<Expr>): Fun {
      return at(k, Fun, σ)
   }

   export class MatchAs extends Expr {
      e: Expr = _
      σ: Trie<Expr> = _
   }

   export function matchAs (k: Id, e: Expr, σ: Trie<Expr>): MatchAs {
      return at(k, MatchAs, e, σ)
   }

   export class Quote extends Expr {
      e: Expr = _
   }

   export function quote (k: Id, e: Expr): Quote {
      return at(k, Quote, e)
   }

   export class Typematch extends Expr {
      e: Expr = _
      cases: FiniteMap<Expr> = _
   }

   export function typematch (k: Id, e: Expr, cases: FiniteMap<Expr>): Typematch {
      return at(k, Typematch, e, cases)
   }

   export class Var extends Expr {
      x: Str = _
   }

   export function var_ (k: Id, x: Str): Var {
      return at(k, Var, x)
   }

   export type Trie<K extends Cont> = Trie.Trie<K>

   export namespace Trie {
      export abstract class Trie<K extends Cont> extends DataValue<"Trie"> {
         static join<K extends Cont> (σ: Trie<K>, τ: Trie<K>): Trie<K> {
            if (Var.is(σ) && Var.is(τ) && eq(σ.x, τ.x)) {
               return var_(σ.x, join(σ.κ, τ.κ))
            } else
            if (Constr.is(σ) && Constr.is(τ)) {
               // Both maps (which are non-empty) can (inductively) be assumed to have keys taken from the 
               // same datatype. Ensure that invariant is preserved:
               const c_σ: string = σ.cases.toArray()[0].fst.val,
                     c_τ: string = τ.cases.toArray()[0].fst.val
               if (ctrToDataType.get(c_σ) !== ctrToDataType.get(c_τ)) {
                  error(`${c_σ} and ${c_τ} are constructors of different datatypes.`)
               }
               return constr(unionWith(σ.cases, τ.cases, join))
            } else {
               return absurd("Undefined join.", σ, τ)
            }
         }
      }

      export class Constr<K extends Cont> extends Trie<K> {
         cases: FiniteMap<K> = _

         static is<K extends Cont> (σ: Trie<K>): σ is Constr<K> {
            return σ instanceof Constr
         }
      }

      export function constr<K extends Cont> (cases: FiniteMap<K>): Constr<K> {
         return make(Constr, cases) as Constr<K>
      }

      // TODO: use annotations on x.
      export class Var<K extends Cont> extends Trie<K> {
         x: Annotated<Str> = _
         κ: K = _

         static is<K extends Cont> (σ: Trie<K>): σ is Var<K> {
            return σ instanceof Var
         }
      }

      export function var_<K extends Cont> (x: Annotated<Str>, κ: K): Var<K> {
         return make(Var, x, κ) as Var<K>
      }
   }
}
