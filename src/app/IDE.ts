import "../BaseTypes"
import "../Graphics2"
import { Direction, __annotations, negateallα } from "../Annotated"
import { Env } from "../Env"
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

   onBwd (editor: Editor.Editor): void {
      // more stuff to do here to make sure program slice is disregarded
      __annotations.reset(Direction.Fwd)
      negateallα(this.ρ_external)
      this.editors.filter(e => e !== editor).forEach(e => e.fwdSlice())
   }
}

initialise()
