import { Placement } from "tippy.js"
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
   // TODO: eliminate redundancy with "renewables" test
   const ide: IDE = new IDE(openDatasetAs("renewables-restricted", "data"))
   const [ρ1, e1]: [Env, Expr] = openWithImports("graphics/grouped-bar-chart")
   const [ρ2, e2]: [Env, Expr] = openWithImports("graphics/stacked-bar-chart")
   const [ρ3, e3]: [Env, Expr] = openWithImports("graphics/line-chart")
   ide.addEditor(ρ1, e1)
   ide.addEditor(ρ2, e2)
   ide.addEditor(ρ3, e3)
}

export class IDE implements Editor.Listener {
   ρ_external: Env
   editors: Editor.Editor[] = []

   constructor (ρ_external: Env) {
      this.ρ_external = ρ_external
      // Wait for fonts to load before rendering, otherwise metrics will be wrong.
      window.onload = (ev: Event) => this.editors.forEach(editor => editor.onLoad(ev))
   }

   addEditor (ρ: Env, e: Expr, tooltipPlacement: Placement = "top"): Editor.Editor {
      const editor: Editor.Editor = new Editor.Editor(this, View.defaultDims, tooltipPlacement, this.ρ_external, ρ, e)
      this.editors.push(editor)
      return editor
   }

   onBwdSlice (editor: Editor.Editor, setNeeded: () => void): void {
      __slice.reset(Direction.Bwd)
      setNeeded()
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
   }
}

initialise()
