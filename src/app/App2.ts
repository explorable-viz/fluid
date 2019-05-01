import { nil } from "../BaseTypes2"
import { eval_ } from "../Eval2"
import { Expr } from "../Expr2"

class App {
   constructor () {
      const e: Expr = Expr.constr("Nil", nil())
      eval_(e)
   }   
}

new App
