import { zipWith } from "./util/Array"
import { absurd, assert } from "./util/Core"
import { AnnotatedC } from "./Annotated"
import { List } from "./BaseTypes"
import { DataValue, ExplValue, explValue } from "./DataValue"
import { Eval } from "./Eval"
import { Expr } from "./Expr"
import { Match } from "./Match"
import { UnaryOp } from "./Primitive"
import { PrimValue, Str, _ } from "./Value"
import { ν, at } from "./Versioned"

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

   export function app (tf: ExplValue<Closure>, tu: ExplValue, δ: List<RecDef>, ξ: Match<Expr>, t: Expl): App {
      return at(ν(), App, tf, tu, δ, ξ, t)
   }

   export class UnaryApp extends Expl {
      tf: ExplValue<UnaryOp> = _
      tv: ExplValue<PrimValue> = _
   }

   export function unaryApp (tf: ExplValue<UnaryOp>, tv: ExplValue<PrimValue>): UnaryApp {
      return at(ν(), UnaryApp, tf, tv)
   }

   export class BinaryApp extends Expl {
      tv1: ExplValue<PrimValue> = _
      opName: Str = _
      tv2: ExplValue<PrimValue> = _
   }

   export function binaryApp (tv1: ExplValue<PrimValue>, opName: Str, tv2: ExplValue<PrimValue>): BinaryApp {
      return at(ν(), BinaryApp, tv1, opName, tv2)
   }

   // Has a concrete subclass for each datatype.
   export class DataExpl extends Expl {
      children (): Expl[] {
         return super.children() as Expl[]
      }
   }   

   export abstract class Def extends DataValue<"Expl.Def"> {
   }

   export class Let extends Def {
      x: Str = _
      tv: ExplValue = _
   }

   export function let_ (x: Str, tv: ExplValue): Let {
      return at(ν(), Let, x, tv)
   }

   export class Prim extends Def {
      x: Str = _
      t_op: ExplValue<UnaryOp> = _
   }

   export function prim (x: Str, t_op: ExplValue<UnaryOp>): Prim {
      return at(ν(), Prim, x, t_op)
   }

   export class RecDef extends DataValue<"Expl.RecDef"> {
      x: Str = _
      tf: ExplValue<Closure> = _
   }

   export function recDef (x: Str, tf: ExplValue<Closure>): RecDef {
      return at(ν(), RecDef, x, tf)
   }

   export class LetRec extends Def {
      δ: List<RecDef> = _
   }

   export function letRec (δ: List<RecDef>): LetRec {
      return at(ν(), LetRec, δ)
   }

   export class Defs extends NonTerminal {
      def̅: List<Def> = _
      t: Expl = _
   }

   export function defs (def̅: List<Def>, t: Expl): Defs {
      return at(ν(), Defs, def̅, t)
   }

   export class Const extends Expl {
   }

   export function const_ (): Const {
      return at(ν(), Const)
   }

   export class MatchAs extends NonTerminal {
      tu: ExplValue = _
      ξ: Match<Expr> = _
      t: Expl = _
   }

   export function matchAs (tu: ExplValue, ξ: Match<Expr>, t: Expl): MatchAs {
      return at(ν(), MatchAs, tu, ξ, t)
   }

   export class Quote extends Expl {
   }

   export function quote (): Quote {
      return at(ν(), Quote)
   }

   export class Typematch extends NonTerminal {
      tu: ExplValue = _
      d: Str = _
      t: Expl = _
   }

   export function typematch (tu: ExplValue, d: Str, t: Expl): Typematch {
      return at(ν(), Typematch, tu, d, t)
   }

   export class Var extends NonTerminal {
      x: Str = _
      t: Expl = _
   }

   export function var_ (x: Str, t: Expl): Var {
      return at(ν(), Var, x, t)
   }

   // Consolidate; perhaps syntactically unify all these "tail-call" forms?
   export function explChild<T extends DataValue> (t: Expl, v: DataValue, k: keyof T): ExplValue {
      if (t instanceof DataExpl) {
         return explValue(t.child(k as string) as Expl, v.child(k as string))
      } else
      if (t instanceof NonTerminal) {
         return explChild(t.t, v, k)
      } else
      // Should probably require primitives to returned explained values.
      if (t instanceof UnaryApp) {
         return absurd()
      } else
      if (t instanceof BinaryApp) {
         return absurd()
      } else {
         return absurd()
      }
   }

   export function explChildren (t: Expl, v: DataValue): ExplValue[] {
      if (t instanceof DataExpl) {
         return zipWith(explValue)(t.children(), v.children())
      } else 
      if (t instanceof NonTerminal) {
         return explChildren(t.t, v)
      } else
      // Should probably require primitives to returned explained values.
      if (t instanceof UnaryApp) {
         assert(v.children().length === 0)
         return []
      } else
      if (t instanceof BinaryApp) {
         assert(v.children().length === 0)
         return []
      } else {
         return absurd()
      }
   }
}
