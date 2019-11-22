import "../BaseTypes"
import "../Graphics2"
import { Env } from "../Env"
import { Expr } from "../Expr"
import { Module, openDatasetAs, openWithImports2 } from "../Module"
import "../app/GraphicsRenderer2"
import { Editor } from "./Editor"

export function initialise(): void {
   Module.initialise()
   const ρ: Env = openDatasetAs("renewables", "data")
   const [ρʹ, e]: [Env, Expr] = openWithImports2("graphics/line-chart")
   new Editor(false, e, ρ.concat(ρʹ))
}

initialise()
