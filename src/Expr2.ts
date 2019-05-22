import { absurd, error } from "./util/Core"
import { eq } from "./util/Ord"
import { List } from "./BaseTypes2"
import { ctrToDataType } from "./DataType2"
import { FiniteMap, unionWith } from "./FiniteMap2"
import { DataValue, Id, Num, Str, _, make } from "./Value2"
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
export type Kont<K> = Expr.Kont<K>

export namespace Expr {
   // It would be nice if (non-argument) tries only had argument tries as their continuations and vice-
   // versa, but that doesn't quite work because a DataValue<K> has an underlying map to Args<K>.
   type KontTag = "Expr" | "Trie" | "Args"

   export class Kont<K, Tag extends KontTag = KontTag> extends DataValue<Tag> {
   }

   // Don't understand how polymorphism interacts with subtyping, so brute-force this instead. 
   // Use the same heinous cast as used in 'instantiateKont'. Note this join is unrelated to the annotation lattice.
   function join<K extends Kont<K>> (κ: K, κʹ: K): K {
      if (κ instanceof Trie.Trie && κʹ instanceof Trie.Trie) {
         return Trie.Trie.join<K>(κ, κʹ) as K
      } else
      if (κ instanceof Args.Args && κʹ instanceof Args.Args) {
         return Args.Args.join<K>(κ, κʹ) as K
      } else {
         return absurd("Undefined join.")
      }
   }

   export abstract class Expr extends VersionedC(Kont)<Expr, "Expr"> {
   }
   
   export class App extends Expr {
      func: Expr = _
      arg: Expr = _
   }

   export function app (k: Id, func: Expr, arg: Expr): App {
      return at(k, App, func, arg)
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

   export type Trie<K extends Kont<K>> = Trie.Trie<K>
   export type Args<K extends Kont<K>> = Args.Args<K>

   export namespace Args {
      export abstract class Args<K extends Kont<K>> extends Kont<Args<K>, "Args"> {
         static join<K extends Kont<K>> (Π: Args<K>, Πʹ: Args<K>): Args<K> {
            if (End.is(Π) && End.is(Πʹ)) {
               return end(join(Π.κ, Πʹ.κ))
            } else
            if (Next.is(Π) && Next.is(Πʹ)) {
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
         return make(End, κ) as End<K>
      }

      export class Next<K extends Kont<K>> extends Args<K> {
         σ: Trie<Args<K>> = _

         static is<K extends Kont<K>> (Π: Args<K>): Π is Next<K> {
            return Π instanceof Next
         }
      }

      export function next<K extends Kont<K>> (σ: Trie<Args<K>>): Next<K> {
         return make(Next, σ)
      }
   }

   export namespace Trie {
      export abstract class Trie<K extends Kont<K>> extends Kont<Trie<K>, "Trie"> {
         static join<K extends Kont<K>> (σ: Trie<K>, τ: Trie<K>): Trie<K> {
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
               return constr(unionWith(σ.cases, τ.cases, Args.Args.join))
            } else {
               return absurd("Undefined join.", this, τ)
            }
         }
      }

      export class Constr<K extends Kont<K>> extends Trie<K> {
         cases: FiniteMap<Args<K>> = _

         static is<K extends Kont<K>> (σ: Trie<K>): σ is Constr<K> {
            return σ instanceof Constr
         }
      }

      export function constr<K extends Kont<K>> (cases: FiniteMap<Args<K>>): Constr<K> {
         return make(Constr, cases)
      }

      // TODO: use Versioned<Str> by analogy with other binding forms.
      export class Var<K extends Kont<K>> extends Trie<K> {
         x: Str = _
         κ: K = _

         static is<K extends Kont<K>> (σ: Trie<K>): σ is Var<K> {
            return σ instanceof Var
         }
      }

      export function var_<K extends Kont<K>> (x: Str, κ: K): Var<K> {
         return make(Var, x, κ) as Var<K>
      }
   }
}
