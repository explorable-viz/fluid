import "../BaseTypes" // need these early because of a Webpack dependency problem
import "../Graphics"
import { Annotated, Direction, __slice } from "../Annotation"
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
      this.editors.push(new Editor.Editor(this, View.defaultDims, "top", this.ρ_external, ρ1, e1))
      this.editors.push(new Editor.Editor(this, View.defaultDims, "right", this.ρ_external, ρ2, e2))
      this.editors.push(new Editor.Editor(this, View.defaultDims, "top", this.ρ_external, ρ3, e3))
      // Wait for fonts to load before rendering, otherwise metrics will be wrong.
      window.onload = (ev: Event) => this.editors.forEach(editor => editor.onload(ev))
   }

   resetForBwd (): void {
      __slice.reset(Direction.Bwd)
   }

   // Returns "external" dependencies identified by the backward slice.
   bwdSlice (editor: Editor.Editor): Set<Annotated> {
      editor.direction = Direction.Bwd
      Eval.eval_bwd(editor.e, editor.tv)
      // consider availability of ρ_external only; treat ρ and e as unlimited resources      
      const externDeps: Set<Annotated> = __slice.restrictTo(this.ρ_external.values())
      __slice.reset(Direction.Fwd)
      this.editors
         .filter(editor_ => editor_ !== editor)
         .forEach((editor_: Editor.Editor): void => {
            __slice.ann = externDeps
            Eval.eval_fwd(editor_.e, editor_.tv)
            __slice.ann = __slice.restrictTo([editor_.tv])
            editor_.direction = Direction.Fwd
            editor_.render() // TODO: just redo selection rendering
         })
      __slice.reset(Direction.Fwd)
      return externDeps
   }
}

initialise()
