import { __nonNull, as } from "../../src/util/Core"
import { ann } from "../../src/util/Lattice"
import { setallα } from "../../src/Annotated"
import { ExplValue } from "../../src/DataValue"
import { __deltas } from "../../src/Delta"
import { Env } from "../../src/Env"
import { Eval } from "../../src/Eval"
import { Expr } from "../../src/Expr"
import { Elim } from "../../src/Match"
import { ν, newRevision, str } from "../../src/Versioned"
import "../../src/Graphics2" // for graphical datatypes
import { ExprCursor, ExplValueCursor } from "../../src/app/Cursor"
import { Editor } from "../../src/app/Editor"
import "../../src/app/GraphicsRenderer" // for graphics primitives

// Helper for extracting a function definition from an environment. Unfortunate that we have to
// create a new string.
export function funDef (ρ: Env, f: string): Elim<Expr> {
   return as(__nonNull(ρ.get(str(f)(ν()))).v, Eval.Closure).f
}

// Key idea here is that we never push slicing further back than ρ (since ρ could potentially
// be supplied by a library function, dataframe in another language, or other resource which
// lacks source code).

export class FwdSlice {
   constructor (ρ: Env, e: Expr) {
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
         new Editor.Editor(e, ρ).render()
      }
   }

   setup (here: ExprCursor): void {
   }

   expect (here: ExplValueCursor): void {
   }
}

export class BwdSlice {
   constructor (ρ: Env, e: Expr) {
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
         new Editor.Editor(e, ρ).render()
      }
   }

   setup (here: ExplValueCursor): void {
   }

   expect (here: ExprCursor): void {      
   }
}

export class Edit {
   constructor (ρ: Env, e: Expr) {
      if (flags.get(Flags.Visualise)) {
         new Editor.Editor(e, ρ).render()
      }
      if (flags.get(Flags.Edit)) {
         Eval.eval_(ρ, e)
         newRevision()
         this.setup(new ExprCursor(e))
         const tv: ExplValue =  Eval.eval_(ρ, e)
         this.expect(ExplValueCursor.descendant(null, tv))
      }
      if (flags.get(Flags.Visualise)) {
         new Editor.Editor(e, ρ).render()
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
