import { __nonNull } from "../../src/util/Core"
import { ann } from "../../src/util/Lattice"
import { setallα } from "../../src/Annotated"
import { ExplValue } from "../../src/DataValue"
import { __deltas } from "../../src/Delta"
import { Env, emptyEnv } from "../../src/Env"
import { Eval } from "../../src/Eval"
import { Expr } from "../../src/Expr"
import { newRevision } from "../../src/Versioned"
import "../../src/Graphics" // for graphical datatypes
import { ExprCursor, ExplValueCursor } from "../../src/app/Cursor"
import { Editor } from "../../src/app/Editor"
import "../../src/app/GraphicsRenderer" // for graphics primitives

// Key idea here is that we never push slicing further back than ρ (since ρ could potentially
// be supplied by a library function, dataframe in another language, or other resource which
// lacks source code).

export class FwdSlice {
   constructor (e: Expr, ρ: Env = emptyEnv()) {
      if (flags.get(Flags.FwdSlice)) {
         newRevision()
         setallα(ann.top, e)
         setallα(ann.top, ρ)
         const tv: ExplValue = Eval.eval_(ρ, e)
         Eval.eval_fwd(e, tv) // slice with full availability first to compute delta
         newRevision()
         this.setup(new ExprCursor(e))
         Eval.eval_fwd(e, tv)
         this.expect(ExplValueCursor.descendant(null, tv))
      }
      if (flags.get(Flags.Visualise)) {
         new Editor(e, ρ).render()
      }
   }

   setup (here: ExprCursor): void {
   }

   expect (here: ExplValueCursor): void {
   }
}

export class BwdSlice {
   constructor (e: Expr, ρ: Env = emptyEnv()) {
      if (flags.get(Flags.BwdSlice)) {
         newRevision()
         setallα(ann.bot, e)
         setallα(ann.bot, ρ)
         const tv: ExplValue = Eval.eval_(ρ, e) // to obtain tv
         Eval.eval_fwd(e, tv) // clear annotations on all values
         newRevision()
         this.setup(ExplValueCursor.descendant(null, tv))
         Eval.eval_bwd(e, tv)
         this.expect(new ExprCursor(e))
      }
      if (flags.get(Flags.Visualise)) {
         new Editor(e, ρ).render()
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
         newRevision()
         this.setup(new ExprCursor(e))
         const tv: ExplValue =  Eval.eval_(ρ, e)
         this.expect(ExplValueCursor.descendant(null, tv))
      }
   }

   setup (here: ExprCursor): void {
   }

   expect (here: ExplValueCursor): void {
   }
}

enum Flags { BwdSlice, FwdSlice, Edit, Visualise }

const flags: Map<Flags, boolean> = new Map([
   [Flags.FwdSlice, true],
   [Flags.BwdSlice, true],
   [Flags.Edit, true],
   [Flags.Visualise, true]
])
