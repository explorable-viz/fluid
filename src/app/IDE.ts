import "../BaseTypes"
import "../Graphics2"
import { Env } from "../Env"
import { Expr } from "../Expr"
import { Module, openDatasetAs, openWithImports } from "../Module"
import "../app/GraphicsRenderer2"
import { Editor } from "./Editor"

export function initialise(): void {
   Module.initialise()
   const ρ: Env = openDatasetAs("renewables", "data")
   const [ρʹ, e]: [Env, Expr] = openWithImports("graphics/line-chart")
   new Editor(e, ρ.concat(ρʹ))
}

initialise()
