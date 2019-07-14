import { Annotated } from "./Annotated"
import { List } from "./BaseTypes"
import { DataValue } from "./DataValue"
import { Eval } from "./Eval"
import { Expr } from "./Expr"
import { Match } from "./Match"
import { UnaryOp } from "./Primitive"
import { Str, Value, _, make } from "./Value"
import { ν, at } from "./Versioned"

export type Closure = Eval.Closure
export type Expl = Expl.Expl

export class ExplValue extends DataValue<"ExplValue"> {
   t: Expl = _
   v: Annotated<Value> = _
}

export function explValue (t: Expl, v: Annotated<Value>): ExplValue {
   return make(ExplValue, t, v)
}

export namespace Expl {
   export abstract class Expl extends DataValue<"Expl"> {
   }

   export class App extends Expl {
      tf: ExplValue = _
      tu: ExplValue = _
      δ: List<RecDef> = _ // additional recursive functions bound at this step
      ξ: Match<Expr> = _
      tv: ExplValue = _
   }

  export function app (tf: ExplValue, tu: ExplValue, δ: List<RecDef>, ξ: Match<Expr>, tv: ExplValue): App {
      return at(ν(), App, tf, tu, δ, ξ, tv)
   }

   export class UnaryApp extends Expl {
      tf: ExplValue = _
      tv: ExplValue = _
   }

   export function unaryApp (tf: ExplValue, tv: ExplValue): UnaryApp {
      return at(ν(), UnaryApp, tf, tv)
   }

   export class BinaryApp extends Expl {
      tv1: ExplValue = _
      opName: Str = _
      tv2: ExplValue = _
   }

   export function binaryApp (tv1: ExplValue, opName: Str, tv2: ExplValue): BinaryApp {
      return at(ν(), BinaryApp, tv1, opName, tv2)
   }

   export abstract class Def extends DataValue<"Expl.Def"> {
   }

   export class Let extends Def {
      x: Annotated<Str> = _
      tv: ExplValue = _
   }

   export function let_ (x: Annotated<Str>, tv: ExplValue): Let {
      return at(ν(), Let, x, tv)
   }

   export class Prim extends Def {
      x: Annotated<Str> = _
      op: Annotated<UnaryOp> = _
   }

   export function prim (x: Annotated<Str>, op: Annotated<UnaryOp>): Prim {
      return at(ν(), Prim, x, op)
   }

   export class RecDef extends DataValue<"Expl.RecDef"> {
      x: Annotated<Str> = _
      f: Closure = _
   }

   export function recDef (x: Annotated<Str>, f: Closure): RecDef {
      return at(ν(), RecDef, x, f)
   }

   export class LetRec extends Def {
      δ: List<RecDef> = _
   }

   export function letRec (δ: List<RecDef>): LetRec {
      return at(ν(), LetRec, δ)
   }

   export class Defs extends Expl {
      def̅: List<Def> = _
      tv: ExplValue = _
   }

   export function defs (def̅: List<Def>, tv: ExplValue): Defs {
      return at(ν(), Defs, def̅, tv)
   }

   export class Empty extends Expl {
   }

   export function empty (): Empty {
      return at(ν(), Empty)
   }

   export class MatchAs extends Expl {
      tu: ExplValue = _
      ξ: Match<Expr> = _
      tv: ExplValue = _
   }

   export function matchAs (tu: ExplValue, ξ: Match<Expr>, tv: ExplValue): MatchAs {
      return at(ν(), MatchAs, tu, ξ, tv)
   }

   export class Quote extends Expl {
   }

   export function quote (): Quote {
      return at(ν(), Quote)
   }

   export class Typematch extends Expl {
      tu: ExplValue = _
      d: Str = _
      tv: ExplValue = _
   }

   export function typematch (tu: ExplValue, d: Str, tv: ExplValue): Typematch {
      return at(ν(), Typematch, tu, d, tv)
   }

   // v is the resolved value of x
   export class Var extends Expl {
      x: Str = _
      v: Annotated<Value> = _
   }

   export function var_ (x: Str, v: Annotated<Value>): Var {
      return at(ν(), Var, x, v)
   }
}
