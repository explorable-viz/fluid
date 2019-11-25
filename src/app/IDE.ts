import "../BaseTypes"
import "../Graphics"
import { Annotated, Direction, __annotations } from "../Annotation"
import { Env } from "../Env"
import { Eval } from "../Eval"
import { Expr } from "../Expr"
import { openDatasetAs, openWithImports } from "../Module"
import "../app/GraphicsRenderer"
import { Editor } from "./Editor"
import { View } from "./View"

export function initialise (): void {
   Editor.initialise()
   new IDE()
}

class IDE implements Editor.Listener {
   ρ_external: Env
   editors: Editor.Editor[] = []

   constructor () {
      this.ρ_external = openDatasetAs("renewables-restricted", "data")
      const [ρ1, e1]: [Env, Expr] = openWithImports("graphics/grouped-bar-chart")
      const [ρ2, e2]: [Env, Expr] = openWithImports("graphics/stacked-bar-chart")
      const [ρ3, e3]: [Env, Expr] = openWithImports("graphics/line-chart")
      this.editors.push(
         new Editor.Editor(this, View.defaultDims, "top", this.ρ_external, ρ1, e1),
         new Editor.Editor(this, View.defaultDims, "right", this.ρ_external, ρ2, e2),
         new Editor.Editor(this, View.defaultDims, "top", this.ρ_external, ρ3, e3)
      )
      // Wait for fonts to load before rendering, otherwise metrics will be wrong.
      window.onload = (ev: Event) => this.editors.forEach(editor => editor.onload(ev))
   }

   resetForBwd (): void {
      __annotations.reset(Direction.Bwd)
   }

   // Returns "external" dependencies identified by the backward slice.
   bwdSlice (editor: Editor.Editor): Set<Annotated> {
      editor.direction = Direction.Bwd
      Eval.eval_bwd(editor.e, editor.tv)
      // consider availability of ρ_external only; treat ρ and e as unlimited resources
      __annotations.restrictTo(this.ρ_external.values())
      const externDeps: Set<Annotated> = new Set(__annotations.ann)
      __annotations.direction = Direction.Fwd
      this.editors.filter(editor_ => editor_ !== editor).forEach((editor_: Editor.Editor): void => {
         Eval.eval_fwd(editor_.e, editor_.tv)
         __annotations.restrictTo([editor_.tv])
         editor_.direction = Direction.Fwd
         editor_.render() // TODO: just redo selection rendering
      })
      return externDeps
   }
}

initialise()
