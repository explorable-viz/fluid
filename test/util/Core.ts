import { __nonNull } from "../../src/util/Core"
import { ann } from "../../src/util/Lattice"
import { setallα } from "../../src/Annotated"
import { ExplValue } from "../../src/DataValue"
import { Env, emptyEnv } from "../../src/Env"
import { Eval } from "../../src/Eval"
import { Expr } from "../../src/Expr"
import { clearDelta, clearMemo } from "../../src/Value"
import "../../src/Graphics" // for graphical datatypes
import { ExprCursor, ExplValueCursor } from "../../src/app/Cursor"
import "../../src/app/GraphicsRenderer" // for graphics primitives

// Key idea here is that we never push slicing further back than ρ (since ρ could potentially
// be supplied by a library function, dataframe in another language, or other resource which
// lacks source code).

export class FwdSlice {
   constructor (e: Expr, ρ: Env = emptyEnv()) {
      clearMemo()
      setallα(ann.top, e)
      setallα(ann.top, ρ)
      const tv: ExplValue = Eval.eval_(ρ, e)
      Eval.eval_fwd(e, tv) // slice with full availability first to compute delta
      clearDelta()
      this.setup(new ExprCursor(e))
      if (flags.get(Flags.Fwd)) {
         Eval.eval_fwd(e, tv)
         this.expect(new ExplValueCursor(tv))
      }
      console.log(e)
      console.log(tv)
   }

   setup (here: ExprCursor): void {
   }

   expect (here: ExplValueCursor): void {
   }
}

export class BwdSlice {
   expr: ExprCursor

   constructor (e: Expr, ρ: Env = emptyEnv()) {
      if (flags.get(Flags.Bwd)) {
         clearMemo()
         setallα(ann.bot, e)
         setallα(ann.bot, ρ)
         const tv: ExplValue = Eval.eval_(ρ, e) // to obtain tv
         Eval.eval_fwd(e, tv) // clear annotations on all values
         clearDelta()
         this.setup(new ExplValueCursor(tv))
         Eval.eval_bwd(e, tv)
         this.expr = new ExprCursor(e)
         this.expect()
      }
   }

   setup (here: ExplValueCursor): void {
   }

   expect (): void {      
   }
}

enum Flags { Bwd, Fwd }
const flags: Map<Flags, boolean> = new Map([
   [Flags.Fwd, true],
   [Flags.Bwd, true]
])
