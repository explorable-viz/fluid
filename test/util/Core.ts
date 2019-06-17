import { __nonNull, as } from "../../src/util/Core"
import { successfulParse } from "../../src/util/parse/Core"
import { ann } from "../../src/util/Annotated"
import { emptyEnv } from "../../src/Env"
import { Eval } from "../../src/Eval"
import { ExplValue } from "../../src/ExplValue"
import { Expr } from "../../src/Expr"
import "../../src/Graphics" // for graphical datatypes
import { importDefaults } from "../../src/Module"
import "../../src/app/GraphicsRenderer" // for graphics primitives
import { Parse } from "../../src/Parse"
import { setallα } from "../../src/Versioned"
import { Cursor } from "./Cursor"

export class FwdSlice {
   expr: Cursor
   val: Cursor

   constructor (e: Expr) {
      setallα(ann.top, e)
      this.expr = new Cursor(e)
      this.setup()
      const tv: ExplValue = Eval.eval_(emptyEnv(), e)
      if (flags.get(Flags.Fwd)) {
         Eval.eval_fwd(tv)
         this.val = new Cursor(tv.v)
         this.expect()
      }
      console.log(e)
      console.log(tv)
   }

   setup (): void {
   }

   expect (): void {
   }

   get e (): Expr {
      return as(this.expr.v, Expr.Expr)
   }
}

export class BwdSlice {
   val: Cursor
   expr: Cursor

   constructor (e: Expr) {
      if (flags.get(Flags.Bwd)) {
         setallα(ann.bot, e)
         const tv: ExplValue = Eval.eval_(emptyEnv(), e) // just to obtain tv
         Eval.eval_fwd(tv) // clear annotations on all values
         this.val = new Cursor(tv.v)
         this.setup()
         this.expr = new Cursor(Eval.eval_bwd(tv))
         this.expect()
      }
   }

   setup (): void {
   }

   expect (): void {      
   }
}

enum Flags { Bwd, Fwd }
const flags: Map<Flags, boolean> = new Map([
   [Flags.Fwd, true],
   [Flags.Bwd, true]
])

export function parse (src: string): Expr {
   return importDefaults(successfulParse(Parse.expr, src))
}
