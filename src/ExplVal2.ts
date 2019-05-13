import { Env } from "./Func2"
import { Expr, Lex } from "./Expr2"
import { List } from "./BaseTypes2"
import { Constr, Value, _, make } from "./Value2"

export namespace Expl {
   export abstract class Expl extends Constr<Expl> {
   }

   export class App extends Expl {
      func: ExplVal = _    // Expl would suffice, but for uneval we need address of function
      arg: ExplVal = _     // Expl would suffice, but more uniform this way
      tv: ExplVal = _      // technically Expl would suffice, but for uneval we want environment
   }

   export function app (func: ExplVal, arg: ExplVal, tv: ExplVal): App {
      return make(App, func, arg, tv)
   }

   export class UnaryApp extends Expl {
      func: ExplVal = _
      arg: ExplVal = _
   }

   export function unaryApp (func: ExplVal, arg: ExplVal): UnaryApp {
      return make(UnaryApp, func, arg)
   }

   export class Empty extends Expl {
   }

   export function empty (): Empty {
      return make(Empty)
   }

   export class Let extends Expl {
      tu: ExplVal = _
      tv: ExplVal = _ // technically Expl would suffice, but for uneval we want environment
   }

   export function let_ (tu: ExplVal, tv: ExplVal): Let {
      return make(Let, tu, tv)
   }

   export class LetRec extends Expl {
      δ: List<Expr.RecDef> = _
      ρ_defs: Env = _     // from closeDefs, for uneval
      tv: ExplVal = _
   }

   export function letRec (δ: List<Expr.RecDef>, ρ_defs: Env, tv: ExplVal): LetRec {
      return make(LetRec, δ, ρ_defs, tv)
   }

   export class MatchAs extends Expl {
      tu: ExplVal = _
      tv: ExplVal = _ // technically Expl would suffice, but for uneval we want environment
   }

   export function matchAs (tu: ExplVal, ξtv: ExplVal): MatchAs {
      return make(MatchAs, tu, ξtv)
   }

   export class BinaryApp extends Expl {
      tv1: ExplVal = _
      opName: Lex.OpName = _
      tv2: ExplVal = _
   }

   export function binaryApp (tv1: ExplVal, opName: Lex.OpName, tv2: ExplVal): BinaryApp {
      return make(BinaryApp, tv1, opName, tv2)
   }

   export class Var extends Expl {
      x: Lex.Var = _
   }

   export function var_ (x: Lex.Var): Var {
      return make(Var, x)
   }
}

type Expl = Expl.Expl

export class ExplVal extends Constr<ExplVal> {
   t: Expl = _
   v: Value = _
}
