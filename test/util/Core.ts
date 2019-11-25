import { __nonNull, as } from "../../src/util/Core"
import { Direction, __annotations } from "../../src/Annotation"
import { ExplValue } from "../../src/DataValue"
import { __deltas } from "../../src/Delta"
import { Env, emptyEnv } from "../../src/Env"
import { Eval } from "../../src/Eval"
import { Expr } from "../../src/Expr"
import { Elim } from "../../src/Match"
import { ν, newRevision, str } from "../../src/Versioned"
import "../../src/Graphics" // for graphical datatypes
import { ExprCursor, ExplValueCursor } from "../../src/app/Cursor"
import { Editor } from "../../src/app/Editor"
import "../../src/app/GraphicsRenderer" // for graphics primitives

// Helper for extracting a function definition from an environment. Unfortunate that we have to
// create a new string.
export function funDef (ρ: Env, f: string): Elim<Expr> {
   return as(__nonNull(ρ.get(str(f)(ν()))).v, Eval.Closure).f
}

const __editorListener: Editor.Listener = new class implements Editor.Listener {
   resetForBwd (): void {
   }

   bwdSlice (editor: Editor.Editor): typeof __annotations.ann {
      return new Set()
   }
}()

// Key idea here is that we never push slicing further back than ρ (since ρ could potentially
// be supplied by a library function, dataframe in another language, or other resource which
// lacks source code).

export class FwdSlice {
   constructor (ρ: Env, e: Expr) {
      if (flags.get(Flags.FwdSlice)) {
         const tv: ExplValue = Eval.eval_(ρ, e)
         __annotations.reset(Direction.Fwd)
         this.setup(new ExprCursor(e))
         Eval.eval_fwd(e, tv)
         this.expect(ExplValueCursor.descendant(null, tv))
      }
      if (flags.get(Flags.Visualise)) {
         const editor = new Editor.Editor(__editorListener, [400, 400], "top", emptyEnv(), ρ, e)
         if (flags.get(Flags.FwdSlice)) {
            editor.direction = Direction.Fwd
         }
         editor.render()
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
         const tv: ExplValue = Eval.eval_(ρ, e) // to obtain tv
         __annotations.reset(Direction.Bwd)
         this.setup(ExplValueCursor.descendant(null, tv))
         Eval.eval_bwd(e, tv)
         this.expect(new ExprCursor(e))
      }
      if (flags.get(Flags.Visualise)) {
         const editor = new Editor.Editor(__editorListener, [400, 400], "top", emptyEnv(), ρ, e)
         if (flags.get(Flags.BwdSlice)) {
            editor.direction = Direction.Bwd
         }
         editor.render()
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
         new Editor.Editor(__editorListener, [400, 400], "top", emptyEnv(), ρ, e).render()
      }
      if (flags.get(Flags.Edit)) {
         Eval.eval_(ρ, e)
         newRevision()
         this.setup(new ExprCursor(e))
         const tv: ExplValue =  Eval.eval_(ρ, e)
         this.expect(ExplValueCursor.descendant(null, tv))
      }
      if (flags.get(Flags.Visualise)) {
         new Editor.Editor(__editorListener, [400, 400], "top", emptyEnv(), ρ, e).render()
      }
   }

   setup (here: ExprCursor): void {
   }

   expect (here: ExplValueCursor): void {
   }
}

enum Flags { BwdSlice, FwdSlice, Edit, Visualise }

const flags: Map<Flags, boolean> = new Map([
   [Flags.FwdSlice, false],
   [Flags.BwdSlice, true],
   [Flags.Edit, true],
   [Flags.Visualise, true]
])
