import { __nonNull } from "../../src/util/Core"
import { ann } from "../../src/util/Lattice"
import { setallα } from "../../src/Annotated"
import { ExplValue } from "../../src/DataValue"
import { Delta, __deltas } from "../../src/Delta"
import { Env, emptyEnv } from "../../src/Env"
import { Eval } from "../../src/Eval"
import { Expr } from "../../src/Expr"
import { clearMemo } from "../../src/Value"
import "../../src/Graphics" // for graphical datatypes
import { ExprCursor, ExplValueCursor } from "../../src/app/Cursor"
import "../../src/app/GraphicsRenderer" // for graphics primitives

// Key idea here is that we never push slicing further back than ρ (since ρ could potentially
// be supplied by a library function, dataframe in another language, or other resource which
// lacks source code).

export class FwdSlice {
   constructor (e: Expr, ρ: Env = emptyEnv()) {
      if (flags.get(Flags.Fwd)) {
         clearMemo()
         __deltas.clear()
         setallα(ann.top, e)
         setallα(ann.top, ρ)
         const tv: ExplValue = Eval.eval_(ρ, e)
         Eval.eval_fwd(e, tv) // slice with full availability first to compute delta
         __deltas.clear()
         this.setup(new ExprCursor(e))
         Eval.eval_fwd(e, tv)
         this.expect(new ExplValueCursor(tv))
         console.log(e)
         console.log(tv)
      }
   }

   setup (here: ExprCursor): void {
   }

   expect (here: ExplValueCursor): void {
   }
}

export class BwdSlice {
   constructor (e: Expr, ρ: Env = emptyEnv()) {
      if (flags.get(Flags.Bwd)) {
         clearMemo()
         __deltas.clear()
         setallα(ann.bot, e)
         setallα(ann.bot, ρ)
         const tv: ExplValue = Eval.eval_(ρ, e) // to obtain tv
         Eval.eval_fwd(e, tv) // clear annotations on all values
         __deltas.clear()
         this.setup(new ExplValueCursor(tv))
         Eval.eval_bwd(e, tv)
         this.expect(new ExprCursor(e))
      }
   }

   setup (here: ExplValueCursor): void {
   }

   expect (here: ExprCursor): void {      
   }
}

export class Edit {
   constructor (e: Expr, ρ: Env = emptyEnv()) {
      if (flags.get(Flags.Edit)) {
         Eval.eval_(ρ, e)
         __deltas.clear()
         this.setup(new ExprCursor(e))
         Eval.eval_(ρ, e)
         this.expect([...__deltas.ẟ̅.entries()].map(([, ẟ]) => ẟ))
         console.log(__deltas.size)
      }
   }

   setup (here: ExprCursor): void {
   }

   expect (ẟ̅ : Delta[]): void {
   }
}

enum Flags { Bwd, Fwd, Edit }
const flags: Map<Flags, boolean> = new Map([
   [Flags.Fwd, true],
   [Flags.Bwd, true],
   [Flags.Edit, true]
])
