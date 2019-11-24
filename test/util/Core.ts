import { __nonNull, as } from "../../src/util/Core"
import { Direction, __annotations } from "../../src/Annotated"
import { ExplValue } from "../../src/DataValue"
import { __deltas } from "../../src/Delta"
import { Env, emptyEnv } from "../../src/Env"
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

const __editorListener: Editor.Listener = new class implements Editor.Listener {
   onBwd (): void {
   }
}()

// Key idea here is that we never push slicing further back than ρ (since ρ could potentially
// be supplied by a library function, dataframe in another language, or other resource which
// lacks source code).

export class FwdSlice {
   constructor (ρ: Env, e: Expr) {
      if (flags.get(Flags.FwdSlice)) {
         __annotations.reset(Direction.Fwd)
         const tv: ExplValue = Eval.eval_(ρ, e)
         this.setup(new ExprCursor(e))
         Eval.eval_fwd(e, tv)
         this.expect(ExplValueCursor.descendant(null, tv))
      }
      if (flags.get(Flags.Visualise)) {
         new Editor.Editor(__editorListener, 400, 400, emptyEnv(), ρ, e).render() // yuk, this copies ρ
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
         __annotations.reset(Direction.Bwd)
         const tv: ExplValue = Eval.eval_(ρ, e) // to obtain tv
         this.setup(ExplValueCursor.descendant(null, tv))
         Eval.eval_bwd(e, tv)
         this.expect(new ExprCursor(e))
      }
      if (flags.get(Flags.Visualise)) {
         new Editor.Editor(__editorListener, 400, 400, emptyEnv(), ρ, e).render()
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
         new Editor.Editor(__editorListener, 400, 400, emptyEnv(), ρ, e).render()
      }
      if (flags.get(Flags.Edit)) {
         Eval.eval_(ρ, e)
         newRevision()
         this.setup(new ExprCursor(e))
         const tv: ExplValue =  Eval.eval_(ρ, e)
         this.expect(ExplValueCursor.descendant(null, tv))
      }
      if (flags.get(Flags.Visualise)) {
         new Editor.Editor(__editorListener, 400, 400, emptyEnv(), ρ, e).render()
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
