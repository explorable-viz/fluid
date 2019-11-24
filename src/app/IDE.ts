import "../BaseTypes"
import "../Graphics2"
import { Direction, __annotations, negateallα } from "../Annotation"
import { Env } from "../Env"
import { Eval } from "../Eval"
import { Expr } from "../Expr"
import { openDatasetAs, openWithImports } from "../Module"
import "../app/GraphicsRenderer2"
import { Editor } from "./Editor"

export function initialise (): void {
   Editor.initialise()
   new IDE()
}

class IDE implements Editor.Listener {
   ρ_external: Env
   editors: Editor.Editor[] = []

   constructor () {
      this.ρ_external = openDatasetAs("renewables", "data")
      const [ρ1, e1]: [Env, Expr] = openWithImports("graphics/line-chart")
      const [ρ2, e2]: [Env, Expr] = openWithImports("graphics/stacked-bar-chart")
      this.editors.push(
         new Editor.Editor(this, 700, 600, this.ρ_external, ρ1, e1),
         new Editor.Editor(this, 700, 600, this.ρ_external, ρ2, e2)
      )
      // Wait for fonts to load before rendering, otherwise metrics will be wrong.
      window.onload = (ev: Event) => this.editors.forEach(editor => editor.onload(ev))
   }

   resetForBwd (): void {
      __annotations.reset(Direction.Bwd)
   }

   bwdSlice (editor: Editor.Editor): void {
      editor.direction = Direction.Bwd
      Eval.eval_bwd(editor.e, editor.tv)
      // TODO: make sure program slice is disregarded
      __annotations.reset(Direction.Fwd)
      negateallα(this.ρ_external)
      this.editors.filter(editor_ => editor_ !== editor).forEach((editor_: Editor.Editor): void => {
      // Consider availability of ρ_external only; treat ρ and e as unlimited resources.
         Eval.eval_fwd(editor_.e, editor_.tv)
         editor_.direction = Direction.Fwd
         editor_.render() // TODO: just redo selection rendering
      })
   }
}

initialise()
