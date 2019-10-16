import { __nonNull, absurd, className } from "./util/Core"
import { AnnotatedC } from "./Annotated"
import { List } from "./BaseTypes"
import { DataValue, ExplValue, explValue } from "./DataValue"
import { Eval } from "./Eval"
import { Expr } from "./Expr"
import { Match } from "./Match"
import { UnaryOp } from "./Primitive"
import { Id, PrimValue, Str, _, fields } from "./Value"
import { at } from "./Versioned"

export type Closure = Eval.Closure
export type Expl = Expl.Expl

export namespace Expl {
   export abstract class Expl extends AnnotatedC(DataValue)<"Expl"> {
   }

   export abstract class NonTerminal extends Expl {
      abstract t: Expl
   }

   export class App extends NonTerminal {
      tf: ExplValue<Closure> = _
      tu: ExplValue = _
      δ: List<RecDef> = _ // additional recursive functions bound at this step
      ξ: Match<Expr> = _
      t: Expl = _
   }

   export function app (tf: ExplValue<Closure>, tu: ExplValue, δ: List<RecDef>, ξ: Match<Expr>, t: Expl): (k: Id) => App {
      return at(App, tf, tu, δ, ξ, t)
   }

   export class UnaryApp extends Expl {
      tf: ExplValue<UnaryOp> = _
      tv: ExplValue<PrimValue> = _
   }

   export function unaryApp (tf: ExplValue<UnaryOp>, tv: ExplValue<PrimValue>): (k: Id) => UnaryApp {
      return at(UnaryApp, tf, tv)
   }

   export class BinaryApp extends Expl {
      tv1: ExplValue<PrimValue> = _
      opName: Str = _
      tv2: ExplValue<PrimValue> = _
   }

   export function binaryApp (tv1: ExplValue<PrimValue>, opName: Str, tv2: ExplValue<PrimValue>): (k: Id) => BinaryApp {
      return at(BinaryApp, tv1, opName, tv2)
   }

   // Has a concrete subclass for each datatype.
   export class DataExpl extends Expl {
      get ctr (): string {
         return className(this)
      }

      get __children (): Expl[] {
         return super.__children as Expl[]
      }
   }

   export abstract class Def extends DataValue<"Expl.Def"> {
   }

   export class Let extends Def {
      x: Str = _
      tv: ExplValue = _
   }

   export function let_ (x: Str, tv: ExplValue): (k: Id) => Let {
      return at(Let, x, tv)
   }

   export class Prim extends Def {
      x: Str = _
      t_op: ExplValue<UnaryOp> = _
   }

   export function prim (x: Str, t_op: ExplValue<UnaryOp>): (k: Id) => Prim {
      return at(Prim, x, t_op)
   }

   export class RecDef extends DataValue<"Expl.RecDef"> {
      x: Str = _
      tf: ExplValue<Closure> = _
   }

   export function recDef (x: Str, tf: ExplValue<Closure>): (k: Id) => RecDef {
      return at(RecDef, x, tf)
   }

   export class LetRec extends Def {
      δ: List<RecDef> = _
   }

   export function letRec (δ: List<RecDef>): (k: Id) => LetRec {
      return at(LetRec, δ)
   }

   export class Defs extends NonTerminal {
      def̅: List<Def> = _
      t: Expl = _
   }

   export function defs (def̅: List<Def>, t: Expl): (k: Id) => Defs {
      return at(Defs, def̅, t)
   }

   export class Const extends Expl {
   }

   export function const_ (): (k: Id) => Const {
      return at(Const)
   }

   export class MatchAs extends NonTerminal {
      tu: ExplValue = _
      ξ: Match<Expr> = _
      t: Expl = _
   }

   export function matchAs (tu: ExplValue, ξ: Match<Expr>, t: Expl): (k: Id) => MatchAs {
      return at(MatchAs, tu, ξ, t)
   }

   export class Quote extends Expl {
   }

   export function quote (): (k: Id) => Quote {
      return at(Quote)
   }

   export class Typematch extends NonTerminal {
      tu: ExplValue = _
      d: Str = _
      t: Expl = _
   }

   export function typematch (tu: ExplValue, d: Str, t: Expl): (k: Id) => Typematch {
      return at(Typematch, tu, d, t)
   }

   export class Var extends NonTerminal {
      x: Str = _
      t: Expl = _
   }

   export function var_ (x: Str, t: Expl): (k: Id) => Var {
      return at(Var, x, t)
   }

   // Should probably do a better job of restricting k to be a bona fide field name.
   export function explChild<T extends DataValue> (t: Expl, v: T, k: keyof T): ExplValue {
      if (t instanceof DataExpl) {
         return explValue(t.__child(k as string) as Expl, v.__child(k as string))
      } else
      if (t instanceof NonTerminal) {
         return explChild(t.t, v, k)
      } else {
         // Primitive applications are currently "terminal" forms, which is technically inconsistent with the fact 
         // that they can return structured data. In practice this doesn't matter because they only return values 
         // like True and False, which have no children. Probably primitives should be non-terminal.
         return absurd()
      }
   }

   export function explChildren (t: Expl, v: DataValue): ExplValue[] {
      return fields(v).map(k => explChild(t, v, k as any))
   }
}
