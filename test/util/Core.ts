import { __nonNull, as } from "../../src/util/Core"
import { ann } from "../../src/util/Lattice"
import { setallα } from "../../src/Annotated"
import { Expl_ } from "../../src/DataValue"
import { Env, emptyEnv } from "../../src/Env"
import { Eval } from "../../src/Eval"
import { Expr } from "../../src/Expr"
import { clearMemo } from "../../src/Value"
import "../../src/Graphics" // for graphical datatypes
import "../../src/app/GraphicsRenderer" // for graphics primitives
import { Cursor } from "./Cursor"

export class FwdSlice {
   expr: Cursor
   tv: Cursor

   constructor (e: Expr, ρ: Env = emptyEnv()) {
      clearMemo()
      setallα(ann.top, e)
      setallα(ann.top, ρ)
      this.expr = new Cursor(e)
      this.setup()
      const tv: Expl_ = Eval.eval_(ρ, e)
      if (flags.get(Flags.Fwd)) {
         Eval.eval_fwd(e, tv)
         this.tv = new Cursor(tv)
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
   tv: Cursor
   expr: Cursor

   constructor (e: Expr, ρ: Env = emptyEnv()) {
      if (flags.get(Flags.Bwd)) {
         clearMemo()
         setallα(ann.bot, e)
         setallα(ann.bot, ρ)
         const tv: Expl_ = Eval.eval_(ρ, e) // just to obtain tv
         Eval.eval_fwd(e, tv) // clear annotations on all values
         this.tv = new Cursor(tv)
         this.setup()
         Eval.eval_bwd(e, tv)
         this.expr = new Cursor(e)
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
