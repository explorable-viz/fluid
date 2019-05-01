import { Expr, eval_, nil } from "../Experiment"

class App {
   constructor () {
      const e: Expr = Expr.constr("Nil", nil())
      eval_(e)
   }   
}

new App
