import { Env } from "./Func2"
import { Expr, Lex } from "./Expr2"
import { List } from "./BaseTypes2"
import { Constr, Value, _, make } from "./Value2"

export namespace Expl {
   export abstract class Expl extends Constr<Expl> {
   }

   export class App extends Expl {
      func: ExplValue = _    // Expl would suffice, but for uneval we need address of function
      arg: ExplValue = _     // Expl would suffice, but more uniform this way
      tv: ExplValue = _      // technically Expl would suffice, but for uneval we want environment
   }

   export function app (func: ExplValue, arg: ExplValue, tv: ExplValue): App {
      return make(App, func, arg, tv)
   }

   export class UnaryApp extends Expl {
      func: ExplValue = _
      arg: ExplValue = _
   }

   export function unaryApp (func: ExplValue, arg: ExplValue): UnaryApp {
      return make(UnaryApp, func, arg)
   }

   export class Empty extends Expl {
   }

   export function empty (): Empty {
      return make(Empty)
   }

   export class Let extends Expl {
      tu: ExplValue = _
      tv: ExplValue = _ // technically Expl would suffice, but for uneval we want environment
   }

   export function let_ (tu: ExplValue, tv: ExplValue): Let {
      return make(Let, tu, tv)
   }

   export class LetRec extends Expl {
      δ: List<Expr.RecDef> = _
      ρ_defs: Env = _     // from closeDefs, for uneval
      tv: ExplValue = _
   }

   export function letRec (δ: List<Expr.RecDef>, ρ_defs: Env, tv: ExplValue): LetRec {
      return make(LetRec, δ, ρ_defs, tv)
   }

   export class MatchAs extends Expl {
      tu: ExplValue = _
      tv: ExplValue = _ // technically Expl would suffice, but for uneval we want environment
   }

   export function matchAs (tu: ExplValue, tv: ExplValue): MatchAs {
      return make(MatchAs, tu, tv)
   }

   export class BinaryApp extends Expl {
      tv1: ExplValue = _
      opName: Lex.OpName = _
      tv2: ExplValue = _
   }

   export function binaryApp (tv1: ExplValue, opName: Lex.OpName, tv2: ExplValue): BinaryApp {
      return make(BinaryApp, tv1, opName, tv2)
   }

   export class Var extends Expl {
      x: Lex.Var = _
   }

   export function var_ (x: string): Var {
      return make(Var, x)
   }
}

type Expl = Expl.Expl

export class ExplValue<T extends Value = Value> extends Constr<ExplValue> {
   t: Expl = _
   v: T = _
}

export function explValue<T extends Value = Value> (t: Expl, v: T): ExplValue<T> {
   return make(ExplValue, t, v) as ExplValue<T>
}
