import { List } from "./BaseTypes2"
import { Env } from "./Env2"
import { ExplId } from "./Eval2"
import { Kont } from "./Expr2"
import { UnaryOp } from "./Primitive2"
import { DataValue, PrimValue, Str, Value, _, make } from "./Value2"
import { Versioned, VersionedC, at } from "./Versioned2"

export namespace Match {
   export class Plug<K extends Kont<K>, M extends Match<K>> extends DataValue<"Plug"> {
      ξ: M = _
      κ: K = _ // fills the single hole in ξ
   }

   export function plug<K extends Kont<K>, M extends Match<K>> (ξ: M, κ: K): Plug<K, M> {
      return make(Plug, ξ, κ) as Plug<K, M>
   }

   export namespace Args {
      export class Plug<K extends Kont<K>, M extends Match<K>> extends DataValue<"Args.Plug"> {
         Ψ: M = _
         κ: K = _ // fills the single hole in Ψ
      }

      export function plug<K extends Kont<K>, M extends Match<K>> (ξ: M, κ: K): Plug<K, M> {
         return make(Plug, ξ, κ) as Plug<K, M>
      }

      export abstract class Match<K> extends Value<"Args.Match"> {
         abstract __unapply (): void
      }
   }

   export abstract class Match<K> extends Value<"Match"> {
      abstract __unapply (): void
   }
}

export namespace Expl {
   export abstract class Expl extends VersionedC(DataValue)<"Expl"> {
   }

   export class App extends Expl {
      f: Versioned<Value> = _   // Expl would suffice, but for uneval we need address of function
      u: Versioned<Value> = _   // Expl would suffice, but more uniform this way
      ρᵟ: Env = _               // from closeDefs, for uneval
      v: Versioned<Value> = _   // TODO: record match
   }

   export function app (k: ExplId, f: Versioned<Value>, u: Versioned<Value>, ρᵟ: Env, v: Versioned<Value>): App {
      return at(k, App, f, u, ρᵟ, v)
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

   // v is the computed value, vʹ is the copy bound to x...urgh.
   export class Let extends Def {
      x: Versioned<Str> = _
      v: Versioned<Value> = _
      vʹ: Versioned<Value> = _
   }

   export function let_ (x: Versioned<Str>, v: Versioned<Value>, vʹ: Versioned<Value>): Let {
      return make(Let, x, v, vʹ)
   }

   // See Let.
   export class Prim extends Def {
      x: Versioned<Str> = _
      v: Value = _ // underlying primitive is not versioned
      vʹ: Versioned<Value> = _
   }

   export function prim (x: Versioned<Str>, v: Value, vʹ: Versioned<Value>): Prim {
      return make(Prim, x, v, vʹ)
   }

   export class LetRec extends Def {
      ρᵟ: Env = _
   }

   export function letRec (ρᵟ: Env): LetRec {
      return make(LetRec, ρᵟ)
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
      v: Versioned<Value> = _ // TODO: record match
   }

   export function matchAs (k: ExplId, u: Versioned<Value>, v: Versioned<Value>): MatchAs {
      return at(k, MatchAs, u, v)
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
