import { Placement } from "tippy.js"
import { assert } from "../util/Core"
import { Direction, Slice, __slice } from "../Annotation"
import { Env } from "../Env"
import { Expr } from "../Expr"
import { Pane } from "./Pane"
import { View } from "./View"

export class PaneCoordinator implements Pane.Listener {
   ρ_external: Env
   panes: Set<Pane.Pane> = new Set()

   constructor (ρ_external: Env) {
      this.ρ_external = ρ_external
   }

   addPane (ρ: Env, e: Expr, tooltipPlacement: Placement = "top"): Pane.Pane {
      const pane: Pane.Pane = new Pane.Pane(this, View.defaultDims, tooltipPlacement, this.ρ_external, ρ, e)
      pane.initialise()
      this.panes.add(pane)
      return pane
   }

   removePane (pane: Pane.Pane): void {
      assert(this.panes.has(pane))
      this.panes.delete(pane)
   }

   onBwdSlice (editor: Pane.Pane, externDeps: Slice): void {
      // consider availability of ρ_external only; treat ρ and e as unlimited resources
      [...this.panes]
         .filter(editor_ => editor_ !== editor)
         .forEach((editor_: Pane.Pane): void => {
            editor_.fwdSlice(externDeps)
            editor_.render() // TODO: just redo selection rendering
         })
      __slice.reset(Direction.Fwd)
   }
}
