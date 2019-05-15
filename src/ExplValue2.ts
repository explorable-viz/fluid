import { Expr } from "./Expr2"
import { List } from "./BaseTypes2"
import { ExplId } from "./Eval2"
import { Constr, PrimValue, Str, Value, _, at } from "./Value2"

export namespace Expl {
   export abstract class Expl extends Constr<"Expl"> {
   }

   export class App extends Expl {
      f: Value<any> = _    // Expl would suffice, but for uneval we need address of function
      v: Value<any> = _     // Expl would suffice, but more uniform this way
      // TODO: record match
   }

   export function app (k: ExplId, f: Value<any>, v: Value<any>): App {
      return at(k, App, f, v)
   }

   export class UnaryApp extends Expl {
      f: Value<any> = _
      v: Value<any> = _
   }

   export function unaryApp (k: ExplId, f: Value<any>, v: PrimValue): UnaryApp {
      return at(k, UnaryApp, f, v)
   }

   export class Empty extends Expl {
   }

   export function empty (k: ExplId): Empty {
      return at(k, Empty)
   }

   export class Let extends Expl {
      u: Value<any> = _
      // TODO: record match
   }

   export function let_ (k: ExplId, u: Value<any>): Let {
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
      u: Value<any> = _
      // TODO: record match
   }

   export function matchAs (k: ExplId, u: Value<any>): MatchAs {
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
export function explValue<T extends Value<any> = Value<any>> (t: Expl, v: T): T {
//   v.__expl = t // TOOD: check single-assignment constraint
   return v
}
