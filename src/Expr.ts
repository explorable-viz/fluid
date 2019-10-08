import { __nonNull, abstractMethodError, absurd, error } from "./util/Core"
import { diff, union } from "./util/Set"
import { eq } from "./util/Ord"
import { AnnotatedC } from "./Annotated"
import { Cons, List, Nil } from "./BaseTypes"
import { DataType, ctrToDataType } from "./DataType"
import { DataValue } from "./DataValue"
import { FiniteMap, unionWith } from "./FiniteMap"
import { DataValueTag, Id, Num, Str, _, make } from "./Value"
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
         return fun(join(κ.σ, κʹ.σ))(ν()) as Expr as K
      } else {
         return absurd("Undefined join.", κ, κʹ)
      }
   }

   export abstract class SyntaxNode<Tag extends DataValueTag = DataValueTag> extends AnnotatedC(DataValue)<Tag> {
   }

   export abstract class Expr extends SyntaxNode<"Expr"> {
   }
   
   export class App extends Expr {
      f: Expr = _
      e: Expr = _
   }

   export function app (f: Expr, e: Expr): (k: Id) => App {
      return at(App, f, e)
   }

   export class BinaryApp extends Expr {
      e1: Expr = _
      opName: Str = _
      e2: Expr = _
   }

   export function binaryApp (e1: Expr, opName: Str, e2: Expr): (k: Id) => BinaryApp {
      return at(BinaryApp, e1, opName, e2)
   }

   export class ConstNum extends Expr {
      val: Num = _
   }
   
   export function constNum (val: Num): (k: Id) => ConstNum {
      return at(ConstNum, val)
   }

   export class ConstStr extends Expr {
      val: Str = _
   }

   export function constStr (val: Str): (k: Id) => ConstStr {
      return at(ConstStr, val)
   }

   // Has a concrete subclass for each datatype.
   export class DataExpr extends Expr {
      get ctr (): string {
         return abstractMethodError(this) // currently reflection requires concrete type here
      }

      __children (): Expr[] {
         return super.__children() as Expr[]
      }
   }
   
   export function dataExpr (c: string, e̅: Expr[]): (k: Id) => DataExpr {
      const d: DataType = __nonNull(ctrToDataType.get(c))
      return at(d.exprC̅.get(c)!, ...e̅)
   }

   export class Def extends SyntaxNode<"Expr.Def"> {
   }

   export class Let extends Def {
      x: Str = _
      e: Expr = _
   }

   export function let_ (x: Str, e: Expr): (k: Id) => Let {
      return at(Let, x, e)
   }

   export class Prim extends Def {
      x: Str = _
   }

   export function prim (x: Str): (k: Id) => Prim {
      return at(Prim, x)
   }

   export class RecDef extends SyntaxNode<"RecDef"> {
      x: Str = _
      σ: Trie<Expr> = _
   }
 
   export function recDef (x: Str, σ: Trie<Expr>): (k: Id) => RecDef {
      return at(RecDef, x, σ)
   }

   export class LetRec extends Def {
      δ: List<RecDef> = _
   }

   export function letRec (δ: List<RecDef>): (k: Id) => LetRec {
      return at(LetRec, δ)
   }

   export class Defs extends Expr {
      def̅: List<Def> = _
      e: Expr = _
   }

   export function defs (def̅: List<Def>, e: Expr): (k: Id) => Defs {
      return at(Defs, def̅, e)
   }

   export class Fun extends Expr {
      σ: Trie<Expr> = _
   }

   export function fun (σ: Trie<Expr>): (k: Id) => Fun {
      return at(Fun, σ)
   }

   export class MatchAs extends Expr {
      e: Expr = _
      σ: Trie<Expr> = _
   }

   export function matchAs (e: Expr, σ: Trie<Expr>): (k: Id) => MatchAs {
      return at(MatchAs, e, σ)
   }

   export class Quote extends Expr {
      e: Expr = _
   }

   export function quote (e: Expr): (k: Id) => Quote {
      return at(Quote, e)
   }

   export class Typematch extends Expr {
      e: Expr = _
      cases: FiniteMap<Expr> = _
   }

   export function typematch (e: Expr, cases: FiniteMap<Expr>): (k: Id) => Typematch {
      return at(Typematch, e, cases)
   }

   export class Var extends Expr {
      x: Str = _
   }

   export function var_ (x: Str): (k: Id) => Var {
      return at(Var, x)
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
               return constr(unionWith<K, FiniteMap<K>>(σ.cases, τ.cases, join))
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
         x: Str = _
         κ: K = _

         static is<K extends Cont> (σ: Trie<K>): σ is Var<K> {
            return σ instanceof Var
         }
      }

      export function var_<K extends Cont> (x: Str, κ: K): Var<K> {
         return make(Var, x, κ) as Var<K>
      }
   }

   // used by Wrattler
   export function freeVars (e: Expr): Set<string> {
      if (e instanceof ConstNum) {
         return new Set()
      } else
      if (e instanceof ConstStr) {
         return new Set()
      } else
      if (e instanceof Fun) {
         return freeVarsTrie(e.σ)
      } else
      if (e instanceof DataExpr) {
         return union(...e.__children().map(freeVars))
      } else
      if (e instanceof Quote) {
         return freeVars(e.e)
      } else
      if (e instanceof Var) {
         return new Set([e.x.val])
      } else
      if (e instanceof App) {
         return union(freeVars(e.f), freeVars(e.e))
      } else
      if (e instanceof BinaryApp) {
         return union(freeVars(e.e1), freeVars(e.e2))
      } else
      if (e instanceof Defs) {
         const [bound, free]: [Set<string>, Set<string>] = freeVarsDefs(e.def̅, new Set())
         return union(diff(freeVars(e.e), bound), free)
      } else
      if (e instanceof MatchAs) {
         return union(freeVars(e.e), freeVarsTrie(e.σ))
      } else
      if (e instanceof Typematch) {
         return union(freeVars(e.e), ...e.cases.toArray().map(({ snd }) => freeVars(snd)))
      } else {
         return absurd()
      }
   }

   function freeVarsCont (κ: Cont): Set<string> {
      if (κ instanceof Expr) {
         return freeVars(κ)
      } else 
      if (κ instanceof Trie.Trie) {
         return freeVarsTrie(κ)
      } else {
         return absurd()
      }
   }

   function freeVarsTrie<K extends Cont> (σ: Trie.Trie<K>): Set<string> {
      if (Trie.Var.is(σ)) {
         return diff(freeVarsCont(σ.κ), new Set([σ.x.val]))
      } else
      if (Trie.Constr.is(σ)) {
         return union(...σ.cases.toArray().map(({ snd }) => freeVarsCont(snd)))
      } else {
         return absurd()
      }
   }

   // (bound, free) vars - not necessarily disjoint, because the defs nest
   function freeVarsDefs (def̅: List<Def>, bound: Set<string>): [Set<string>, Set<string>] {
      if (Cons.is(def̅)) {
         const def: Def = def̅.head
         if (def instanceof Prim) {
            const x̅: Set<string> = new Set([def.x.val]),
                  [boundʹ, free] = freeVarsDefs(def̅.tail, bound)
            return [boundʹ, diff(free, x̅)]
         } else
         if (def instanceof Let) {
            const x̅: Set<string> = new Set([def.x.val]),
                  [boundʹ, free] = freeVarsDefs(def̅.tail, union(bound, x̅))
            return [boundʹ, union(diff(free, x̅), freeVars(def.e))]
         } else
         if (def instanceof LetRec) {
            const f̅: RecDef[] = def.δ.toArray(),
                  x̅: Set<string> = new Set(f̅.map(f => f.x.val)),
                  [boundʹ, free] = freeVarsDefs(def̅.tail, union(bound, x̅))
            return [boundʹ, diff(union(free, ...f̅.map(f => freeVarsTrie(f.σ))), x̅)]
         } else {
            return absurd()
         }
      } else
      if (Nil.is(def̅)) {
         return [bound, new Set()]
      } else {
         return absurd()
      }
   }
}
