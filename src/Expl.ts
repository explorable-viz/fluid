import { AnnotatedC } from "./Annotated"
import { List } from "./BaseTypes"
import { DataValue, Expl_ } from "./DataValue"
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

   export class App extends Expl {
      tf: Expl_<Closure> = _
      tu: Expl_ = _
      δ: List<RecDef> = _ // additional recursive functions bound at this step
      ξ: Match<Expr> = _
      tv: Expl_ = _
   }

  export function app (tf: Expl_<Closure>, tu: Expl_, δ: List<RecDef>, ξ: Match<Expr>, tv: Expl_): App {
      return at(ν(), App, tf, tu, δ, ξ, tv)
   }

   export class UnaryApp extends Expl {
      tf: Expl_<UnaryOp> = _
      tv: Expl_<PrimValue> = _
   }

   export function unaryApp (tf: Expl_<UnaryOp>, tv: Expl_<PrimValue>): UnaryApp {
      return at(ν(), UnaryApp, tf, tv)
   }

   export class BinaryApp extends Expl {
      tv1: Expl_<PrimValue> = _
      opName: Str = _
      tv2: Expl_<PrimValue> = _
   }

   export function binaryApp (tv1: Expl_<PrimValue>, opName: Str, tv2: Expl_<PrimValue>): BinaryApp {
      return at(ν(), BinaryApp, tv1, opName, tv2)
   }

   export abstract class Def extends DataValue<"Expl.Def"> {
   }

   export class Let extends Def {
      x: Str = _
      tv: Expl_ = _
   }

   export function let_ (x: Str, tv: Expl_): Let {
      return at(ν(), Let, x, tv)
   }

   export class Prim extends Def {
      x: Str = _
      t_op: Expl_<UnaryOp> = _
   }

   export function prim (x: Str, t_op: Expl_<UnaryOp>): Prim {
      return at(ν(), Prim, x, t_op)
   }

   export class RecDef extends DataValue<"Expl.RecDef"> {
      x: Str = _
      tf: Expl_<Closure> = _
   }

   export function recDef (x: Str, tf: Expl_<Closure>): RecDef {
      return at(ν(), RecDef, x, tf)
   }

   export class LetRec extends Def {
      δ: List<RecDef> = _
   }

   export function letRec (δ: List<RecDef>): LetRec {
      return at(ν(), LetRec, δ)
   }

   export class Defs extends Expl {
      def̅: List<Def> = _
      tv: Expl_ = _
   }

   export function defs (def̅: List<Def>, tv: Expl_): Defs {
      return at(ν(), Defs, def̅, tv)
   }

   export class Empty extends Expl {
   }

   export function empty (): Empty {
      return at(ν(), Empty)
   }

   export class MatchAs extends Expl {
      tu: Expl_ = _
      ξ: Match<Expr> = _
      tv: Expl_ = _
   }

   export function matchAs (tu: Expl_, ξ: Match<Expr>, tv: Expl_): MatchAs {
      return at(ν(), MatchAs, tu, ξ, tv)
   }

   export class Quote extends Expl {
   }

   export function quote (): Quote {
      return at(ν(), Quote)
   }

   export class Typematch extends Expl {
      tu: Expl_ = _
      d: Str = _
      tv: Expl_ = _
   }

   export function typematch (tu: Expl_, d: Str, tv: Expl_): Typematch {
      return at(ν(), Typematch, tu, d, tv)
   }

   export class Var extends Expl {
      x: Str = _
      tv: Expl_ = _
   }

   export function var_ (x: Str, tv: Expl_): Var {
      return at(ν(), Var, x, tv)
   }
}
