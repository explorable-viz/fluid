import "../BaseTypes"
import "../Graphics2"
import { Env } from "../Env"
import { Expr } from "../Expr"
import { openDatasetAs, openWithImports } from "../Module"
import "../app/GraphicsRenderer2"
import { Editor } from "./Editor"

export function initialise(): void {
   Editor.initialise()
   const [ρʹ, e]: [Env, Expr] = openWithImports("graphics/line-chart")
   new Editor.Editor(openDatasetAs("renewables", "data"), ρʹ, e)
}

initialise()
