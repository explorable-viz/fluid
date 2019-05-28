import { absurd, error } from "./util/Core"
import { eq } from "./util/Ord"
import { List } from "./BaseTypes2"
import { ctrToDataType } from "./DataType2"
import { DataValue } from "./DataValue2"
import { FiniteMap, unionWith } from "./FiniteMap2"
import { Id, Num, Str, _, make } from "./Value2"
import { Versioned, VersionedC, at } from "./Versioned2"

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
}

export type Expr = Expr.Expr
export type Kont = Expr.Kont

export namespace Expr {
   // Use to be a parameterised class but we can simplify using our nominal type idiom.
   export type Kont = Expr | DataValue<"Trie">

   // Don't understand how polymorphism interacts with subtyping, so brute-force this instead. 
   // Use the same heinous cast as used in 'instantiateKont'. This join is unrelated to the annotation lattice;
   // the Expr case is intentionally undefined.
   function join<K extends Kont> (κ: K, κʹ: K): K {
      if (κ instanceof Trie.Trie && κʹ instanceof Trie.Trie) {
         return Trie.Trie.join<K>(κ, κʹ) as K
      } else {
         return absurd("Undefined join.")
      }
   }

   export abstract class Expr extends VersionedC(DataValue)<"Expr"> {
   }
   
   export class App extends Expr {
      f: Expr = _
      e: Expr = _
   }

   export function app (k: Id, f: Expr, e: Expr): App {
      return at(k, App, f, e)
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

   // Because let/letrec no longer have "bodies", there's no real need for them to be separately versioned;
   // the variables they introduce are.
   export class Def extends DataValue<"Expr.Def"> {
   }

   export class Let extends Def {
      x: Versioned<Str> = _
      e: Expr = _
   }

   export function let_ (x: Versioned<Str>, e: Expr): Let {
      return make(Let, x, e)
   }

   export class Prim extends Def {
      x: Versioned<Str> = _
   }

   export function prim (x: Versioned<Str>): Prim {
      return make(Prim, x)
   }

   export class RecDef extends DataValue<"RecDef"> {
      x: Versioned<Str> = _
      σ: Trie<Expr> = _
   }
 
   export function recDef (x: Versioned<Str>, σ: Trie<Expr>): RecDef {
      return make(RecDef, x, σ)
   }

   export class LetRec extends Def {
      δ: List<RecDef> = _
   }

   export function letRec (δ: List<RecDef>): LetRec {
      return make(LetRec, δ)
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

   export class BinaryApp extends Expr {
      e1: Expr = _
      opName: Str = _
      e2: Expr = _
   }

   export function binaryApp (k: Id, e1: Expr, opName: Str, e2: Expr): BinaryApp {
      return at(k, BinaryApp, e1, opName, e2)
   }

   export class Var extends Expr {
      x: Str = _
   }

   export function var_ (k: Id, x: Str): Var {
      return at(k, Var, x)
   }

   export type Trie<K extends Kont> = Trie.Trie<K>

   export namespace Trie {
      export abstract class Trie<K extends Kont> extends DataValue<"Trie"> {
         static join<K extends Kont> (σ: Trie<K>, τ: Trie<K>): Trie<K> {
            if (Var.is(σ) && Var.is(τ) && eq(σ.x.val, τ.x.val)) {
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
               return absurd("Undefined join.", this, τ)
            }
         }
      }

      export class Constr<K extends Kont> extends Trie<K> {
         cases: FiniteMap<K> = _

         static is<K extends Kont> (σ: Trie<K>): σ is Constr<K> {
            return σ instanceof Constr
         }
      }

      export function constr<K extends Kont> (cases: FiniteMap<K>): Constr<K> {
         return make(Constr, cases) as Constr<K>
      }

      // TODO: use Versioned<Str> by analogy with other binding forms.
      export class Var<K extends Kont> extends Trie<K> {
         x: Str = _
         κ: K = _

         static is<K extends Kont> (σ: Trie<K>): σ is Var<K> {
            return σ instanceof Var
         }
      }

      export function var_<K extends Kont> (x: Str, κ: K): Var<K> {
         return make(Var, x, κ) as Var<K>
      }
   }
}
