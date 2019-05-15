import { Expr } from "./Expr2"
import { List } from "./BaseTypes2"
import { ExplId } from "./Eval2"
import { Constr, PrimValue, Str, Value, _ } from "./Value2"
import { at } from "./Versioned2"

export namespace Expl {
   export abstract class Expl extends Constr<"Expl"> {
   }

   export class App extends Expl {
      f: Value = _    // Expl would suffice, but for uneval we need address of function
      v: Value = _     // Expl would suffice, but more uniform this way
      // TODO: record match
   }

   export function app (k: ExplId, f: Value, v: Value): App {
      return at(k, App, f, v)
   }

   export class UnaryApp extends Expl {
      f: Value = _
      v: Value = _
   }

   export function unaryApp (k: ExplId, f: Value, v: PrimValue): UnaryApp {
      return at(k, UnaryApp, f, v)
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
      // TODO: record match
   }

   export function matchAs (k: ExplId, u: Value): MatchAs {
      return at(k, MatchAs, u)
   }

   export class BinaryApp extends Expl {
      v1: PrimValue = _
      opName: Str = _
      v2: PrimValue = _
   }

   export function binaryApp (k: ExplId, v1: PrimValue, opName: Str, v2: PrimValue): BinaryApp {
      return at(k, BinaryApp, v1, opName, v2)
   }

   export class Var extends Expl {
      x: Str = _
   }

   export function var_ (k: ExplId, x: Str): Var {
      return at(k, Var, x)
   }
}

type Expl = Expl.Expl

// TODO: this should take a versioned, not a value.
export function explValue<T extends Value = Value> (t: Expl, v: T): T {
//   v.__expl = t // TOOD: check single-assignment constraint
   return v
}
