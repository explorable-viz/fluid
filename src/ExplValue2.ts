import { Expr } from "./Expr2"
import { List } from "./BaseTypes2"
import { DataValue } from "./DataType2"
import { Env } from "./Env2"
import { ExplId } from "./Eval2"
import { UnaryOp } from "./Primitive2"
import { PrimValue, Str, Value, _, make } from "./Value2"
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

   export abstract class Def extends DataValue<"Expl.Def"> {
   }

   export class Let extends Def {
      x: Versioned<Str> = _
      v: Versioned<Value> = _
   }

   export function let_ (x: Versioned<Str>, v: Versioned<Value>): Let {
      return make(Let, x, v)
   }

   export class Prim extends Def {
      x: Versioned<Str> = _
      v: Value = _ // underlying primitive is not versioned
   }

   export function prim (x: Versioned<Str>, v: Value): Prim {
      return make(Prim, x, v)
   }

   export class LetRec extends Def {
      ρ_δ: Env = _
   }

   export function letRec (ρ_δ: Env): LetRec {
      return make(LetRec, ρ_δ)
   }

   export class Defs extends Expl {
      def̅: List<Def> = _
      v: Versioned<Value> = _
   }

   export function defs (k: ExplId, def̅: List<Def>, v: Versioned<Value>): Defs {
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
