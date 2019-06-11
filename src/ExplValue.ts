import { List } from "./BaseTypes"
import { DataValue } from "./DataValue"
import { Eval, ExplId } from "./Eval"
import { Match } from "./Match"
import { UnaryOp } from "./Primitive"
import { Str, Value, _, make } from "./Value"
import { Versioned, VersionedC, at } from "./Versioned"

export type Closure = Eval.Closure
export type Expl = Expl.Expl

export class ExplValue extends DataValue<"ExplValue"> {
   t: Expl = _
   v: Versioned<Value> = _
}

export function explValue (t: Expl, v: Versioned<Value>): ExplValue {
   return make(ExplValue, t, v)
}

export namespace Expl {
   export abstract class Expl extends VersionedC(DataValue)<"Expl"> {
   }

   export class App extends Expl {
      tf: ExplValue = _
      tu: ExplValue = _
      δ: List<RecDef> = _ // additional recursive functions bound at this step
      ξ: Match = _
      tv: ExplValue = _
   }

  export function app (k: ExplId, tf: ExplValue, tu: ExplValue, δ: List<RecDef>, ξ: Match, tv: ExplValue): App {
      return at(k, App, tf, tu, δ, ξ, tv)
   }

   export class UnaryApp extends Expl {
      tf: ExplValue = _
      tv: ExplValue = _
   }

   export function unaryApp (k: ExplId, tf: ExplValue, tv: ExplValue): UnaryApp {
      return at(k, UnaryApp, tf, tv)
   }

   export class BinaryApp extends Expl {
      tv1: ExplValue = _
      opName: Str = _
      tv2: ExplValue = _
   }

   export function binaryApp (k: ExplId, tv1: ExplValue, opName: Str, tv2: ExplValue): BinaryApp {
      return at(k, BinaryApp, tv1, opName, tv2)
   }

   export abstract class Def extends DataValue<"Expl.Def"> {
   }

   export class Let extends Def {
      x: Versioned<Str> = _
      tv: ExplValue = _
   }

   export function let_ (x: Versioned<Str>, tv: ExplValue): Let {
      return make(Let, x, tv)
   }

   export class Prim extends Def {
      x: Versioned<Str> = _
      op: Versioned<UnaryOp> = _
   }

   export function prim (x: Versioned<Str>, op: Versioned<UnaryOp>): Prim {
      return make(Prim, x, op)
   }

   export class RecDef extends DataValue<"Expl.RecDef"> {
      x: Versioned<Str> = _
      f: Closure = _
   }

   export function recDef (x: Versioned<Str>, f: Closure): RecDef {
      return make(RecDef, x, f)
   }

   export class LetRec extends Def {
      δ: List<RecDef> = _
   }

   export function letRec (δ: List<RecDef>): LetRec {
      return make(LetRec, δ)
   }

   export class Defs extends Expl {
      def̅: List<Def> = _
      tv: ExplValue = _
   }

   export function defs (k: ExplId, def̅: List<Def>, tv: ExplValue): Defs {
      return at(k, Defs, def̅, tv)
   }

   export class Empty extends Expl {
   }

   export function empty (k: ExplId): Empty {
      return at(k, Empty)
   }

   export class MatchAs extends Expl {
      tu: ExplValue = _
      ξ: Match = _
      tv: ExplValue = _
   }

   export function matchAs (k: ExplId, tu: ExplValue, ξ: Match, tv: ExplValue): MatchAs {
      return at(k, MatchAs, tu, ξ, tv)
   }

   export class Quote extends Expl {
   }

   export function quote (k: ExplId): Quote {
      return at(k, Quote)
   }

   // v is the resolved value of x
   export class Var extends Expl {
      x: Str = _
      v: Versioned<Value> = _
   }

   export function var_ (k: ExplId, x: Str, v: Versioned<Value>): Var {
      return at(k, Var, x, v)
   }
}
