import { Expr } from "./Expr2"
import { List } from "./BaseTypes2"
import { DataValue } from "./DataType2"
import { Env } from "./Env2"
import { ExplId } from "./Eval2"
import { UnaryOp } from "./Primitive2"
import { Id, PrimValue, Str, Value, _ } from "./Value2"
import { Versioned, VersionedC, at } from "./Versioned2"

export namespace Expl {
   export abstract class Expl extends VersionedC(DataValue)<"Expl"> {
   }

   export class App extends Expl {
      f: Versioned<Value> = _   // Expl would suffice, but for uneval we need address of function
      u: Versioned<Value> = _   // Expl would suffice, but more uniform this way
      ρ_δ: Env = _              // from closeDefs, for uneval
      v: Versioned<Value> = _   // TODO: record match
   }

   export function app (k: ExplId, f: Versioned<Value>, u: Versioned<Value>, ρ_δ: Env, v: Versioned<Value>): App {
      return at(k, App, f, u, ρ_δ, v)
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

   export abstract class Def extends VersionedC(DataValue)<"Def"> {
   }

   export class Let extends Def {
      x: Str = _
      v: Versioned<Value> = _
   }

   export function let_ (k: Id, x: Str, v: Versioned<Value>): Let {
      return at(k, Let, x, v)
   }

   export class Prim extends Def {
   }

   export class LetRec extends Def {
      δ: List<Expr.RecDef> = _
      // TODO: ρ_defs for uneval?
   }

   export function letRec (k: ExplId, δ: List<Expr.RecDef>): LetRec {
      return at(k, LetRec, δ)
   }

   export class Defs extends Expl {
      def̅: List<Def> = _
      v: Versioned<Value> = _
   }

   export function defs (k: ExplId, def̅: List<Def>, v: Expl): Defs {
      return at(k, Defs, def̅, v)
   }

   export class Empty extends Expl {
   }

   export function empty (k: ExplId): Empty {
      return at(k, Empty)
   }

   export class MatchAs extends Expl {
      u: Versioned<Value> = _
      v: Versioned<Value> = _    // TODO: record match
   }

   export function matchAs (k: ExplId, u: Versioned<Value>, v: Versioned<Value>): MatchAs {
      return at(k, MatchAs, u, v)
   }

   export class Var extends Expl {
      x: Str = _
   }

   export function var_ (k: ExplId, x: Str): Var {
      return at(k, Var, x)
   }
}
