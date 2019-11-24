import "../BaseTypes"
import "../Graphics2"
import { Env } from "../Env"
import { Expr } from "../Expr"
import { openDatasetAs, openWithImports } from "../Module"
import "../app/GraphicsRenderer2"
import { Editor } from "./Editor"

export function initialise(): void {
   Editor.initialise()
   new IDE()
}

class IDE {
   constructor () {
      const ρ_external: Env = openDatasetAs("renewables", "data")
      const [ρ1, e1]: [Env, Expr] = openWithImports("graphics/line-chart")
      const [ρ2, e2]: [Env, Expr] = openWithImports("graphics/stacked-bar-chart")
      new Editor.Editor(ρ_external, ρ1, e1)
      new Editor.Editor(ρ_external, ρ2, e2)
   }
}

initialise()
