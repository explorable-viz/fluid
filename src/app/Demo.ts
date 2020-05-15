import { __nonNull } from "../util/Core"
import "../BaseTypes" // need these early because of a Webpack dependency problem
import "../Graphics"
import { __slice } from "../Annotation"
import { Env } from "../Env"
import { Expr } from "../Expr"
import { openDatasetAs, openWithImports } from "../Module"
import "../app/GraphicsRenderer"
import { Pane } from "./Pane"
import { PaneCoordinator } from "./PaneCoordinator"

export function initialise (): void {
   Pane.initialise(".")
   // TODO: eliminate redundancy with "renewables" test
   const appRoot: HTMLElement = __nonNull(document.getElementById("app-root"))
   const coordinator: PaneCoordinator = new PaneCoordinator(appRoot, openDatasetAs("renewables-restricted", "data"))
   const [ρ1, e1]: [Env, Expr] = openWithImports("graphics/grouped-bar-chart")
   const [ρ2, e2]: [Env, Expr] = openWithImports("graphics/stacked-bar-chart")
   const [ρ3, e3]: [Env, Expr] = openWithImports("graphics/line-chart")
   // Wait for fonts to load before rendering, otherwise metrics will be wrong.
   window.onload = (ev: Event): void => {
      coordinator.addPane(ρ1, e1)
      coordinator.addPane(ρ2, e2)
      coordinator.addPane(ρ3, e3)
   }
}

initialise()
