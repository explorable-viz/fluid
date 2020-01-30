import "../BaseTypes" // need these early because of a Webpack dependency problem
import "../Graphics"
import { __slice } from "../Annotation"
import { Env } from "../Env"
import { Expr } from "../Expr"
import { openDatasetAs, openWithImports } from "../Module"
import "../app/GraphicsRenderer"
import { Editor } from "./Editor"
import { EditorCoordinator } from "./EditorCoordinator"

export function initialise (): void {
   Editor.initialise()
   // TODO: eliminate redundancy with "renewables" test
   const coordinator: EditorCoordinator = new EditorCoordinator(openDatasetAs("renewables-restricted", "data"))
   const [ρ1, e1]: [Env, Expr] = openWithImports("graphics/grouped-bar-chart")
   const [ρ2, e2]: [Env, Expr] = openWithImports("graphics/stacked-bar-chart")
   const [ρ3, e3]: [Env, Expr] = openWithImports("graphics/line-chart")
   coordinator.addEditor(ρ1, e1)
   coordinator.addEditor(ρ2, e2)
   coordinator.addEditor(ρ3, e3)
}

initialise()
