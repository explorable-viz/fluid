import { __nonNull, absurd, className } from "./util/Core"
import { diff, union } from "./util/Set"
import { AnnotatedC } from "./Annotated"
import { Cons, List, Nil } from "./BaseTypes"
import { DataValue } from "./DataValue"
import { FiniteMap } from "./FiniteMap"
import { Elim, DataElim, VarElim } from "./Match"
import { DataValueTag, Id, Num, Str, _ } from "./Value"
import { at } from "./Versioned"

// Constants used for parsing, and also for toString() implementations.
export const strings = {
   arrow: "→",
   as: "as",
   bracketL: "[",
   bracketR: "]",
   comma: ",",
   curlyL: "{",
   curlyR: "}",
   ellipsis: "...",
   equals: "=",
   fun: "fun",
   in_: "in",
   let_: "let",
   letRec: "letrec",
   match: "match",
   primitive: "primitive",
   parenL: "(",
   parenR: ")",
   quotes: '"',
   semicolon: ";",
   typematch: "typematch"
}

export type Expr = Expr.Expr
export type Cont = Expr.Cont

export namespace Expr {
   // Use to be a parameterised class but we can simplify using our nominal type idiom.
   export type Cont = Expr | DataValue<"Elim">

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
         return className(this)
      }

      get __children (): Expr[] {
         return super.__children as Expr[]
      }
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
      σ: Elim<Expr> = _
   }
 
   export function recDef (x: Str, σ: Elim<Expr>): (k: Id) => RecDef {
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
      σ: Elim<Expr> = _
   }

   export function fun (σ: Elim<Expr>): (k: Id) => Fun {
      return at(Fun, σ)
   }

   export class MatchAs extends Expr {
      e: Expr = _
      σ: Elim<Expr> = _
   }

   export function matchAs (e: Expr, σ: Elim<Expr>): (k: Id) => MatchAs {
      return at(MatchAs, e, σ)
   }

   export class Quote extends Expr {
      e: Expr = _
   }

   export function quote (e: Expr): (k: Id) => Quote {
      return at(Quote, e)
   }

   // Bring in line with the current eliminator design, i.e. optimise into an object?
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

   // used by Wrattler
   export function freeVars (e: Expr): Set<string> {
      if (e instanceof ConstNum) {
         return new Set()
      } else
      if (e instanceof ConstStr) {
         return new Set()
      } else
      if (e instanceof Fun) {
         return freeVarsElim(e.σ)
      } else
      if (e instanceof DataExpr) {
         return union(...e.__children.map(freeVars))
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
         return union(freeVars(e.e), freeVarsElim(e.σ))
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
      if (κ instanceof Elim) {
         return freeVarsElim(κ)
      } else {
         return absurd()
      }
   }

   function freeVarsElim<K extends Cont> (σ: Elim<K>): Set<string> {
      if (VarElim.is(σ)) {
         return diff(freeVarsCont(σ.κ), new Set([σ.x.val]))
      } else
      if (DataElim.is(σ)) {
         return union(...(σ.__children as K[]).map(freeVarsCont))
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
            return [boundʹ, diff(union(free, ...f̅.map(f => freeVarsElim(f.σ))), x̅)]
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
