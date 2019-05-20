import { Expr } from "./Expr2"
import { List } from "./BaseTypes2"
import { DataValue } from "./DataType2"
import { ExplId } from "./Eval2"
import { UnaryOp } from "./Primitive2"
import { PrimValue, Str, Value, _ } from "./Value2"
import { Versioned, at } from "./Versioned2"

export namespace Expl {
   export abstract class Expl extends DataValue<"Expl"> {
   }

   export class App extends Expl {
      f: Value = _   // Expl would suffice, but for uneval we need address of function
      v: Value = _   // Expl would suffice, but more uniform this way
      t: Expl = _    // TODO: record match
   }

   export function app (k: ExplId, f: Value, v: Value, t: Expl): App {
      return at(k, App, f, v, t)
   }

   export class UnaryApp extends Expl {
      f: Versioned<UnaryOp> = _
      v: Versioned<PrimValue> = _
   }

   export function unaryApp (k: ExplId, f: Versioned<UnaryOp>, v: Versioned<PrimValue>): UnaryApp {
      return at(k, UnaryApp, f, v)
   }

   export class BinaryApp extends Expl {
      v1: Versioned<PrimValue> = _
      opName: Str = _
      v2: Versioned<PrimValue> = _
   }

   export function binaryApp (k: ExplId, v1: Versioned<PrimValue>, opName: Str, v2: Versioned<PrimValue>): BinaryApp {
      return at(k, BinaryApp, v1, opName, v2)
   }

   export class Defs extends Expl {
      // TODO: record evaluated defs
      t: Expl = _
   }

   export function defs (k: ExplId, t: Expl): Defs {
      return at(k, Defs, t)
   }

   export class Empty extends Expl {
   }

   export function empty (k: ExplId): Empty {
      return at(k, Empty)
   }

   export class Let extends Expl {
      u: Value = _
      // TODO: record match
   }

   export function let_ (k: ExplId, u: Value): Let {
      return at(k, Let, u)
   }

   export class LetRec extends Expl {
      δ: List<Expr.RecDef> = _
      // TODO: ρ_defs for uneval?
   }

   export function letRec (k: ExplId, δ: List<Expr.RecDef>): LetRec {
      return at(k, LetRec, δ)
   }

   export class MatchAs extends Expl {
      u: Value = _
      t: Expl = _    // TODO: record match
   }

   export function matchAs (k: ExplId, u: Value, t: Expl): MatchAs {
      return at(k, MatchAs, u, t)
   }

   export class Var extends Expl {
      x: Str = _
   }

   export function var_ (k: ExplId, x: Str): Var {
      return at(k, Var, x)
   }
}
