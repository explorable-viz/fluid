import "../BaseTypes"
import "../Graphics2"
import { __annotations } from "../Annotated"
import { Env } from "../Env"
import { Expr } from "../Expr"
import { openDatasetAs, openWithImports } from "../Module"
import "../app/GraphicsRenderer2"
import { Editor } from "./Editor"

export function initialise (): void {
   new IDE()
   Editor.initialise()
}

class IDE {
   editors: Editor.Editor[] = []

   constructor () {
      const ρ_external: Env = openDatasetAs("renewables", "data")
      const [ρ1, e1]: [Env, Expr] = openWithImports("graphics/line-chart")
      const [ρ2, e2]: [Env, Expr] = openWithImports("graphics/stacked-bar-chart")
      this.editors.push(
         new Editor.Editor(700, 600, ρ_external, ρ1, e1),
         new Editor.Editor(700, 600, ρ_external, ρ2, e2)
      )
      // Wait for fonts to load before rendering, otherwise metrics will be wrong.
      window.onload = (ev: Event) => this.editors.forEach(editor => editor.onload(ev))
   }
}

initialise()
