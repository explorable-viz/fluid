import { Instance as Tooltip, Placement } from "tippy.js"
import { __nonNull, as } from "../util/Core"
import { Direction, Slice, __slice } from "../Annotation"
import { DataValue, ExplValue, explValue } from "../DataValue"
import { __deltas } from "../Delta"
import { Env } from "../Env"
import { Eval } from "../Eval"
import { Expl } from "../Expl"
import { Expr } from "../Expr"
import { Arrowhead } from "../Graphics"
import { newRevision } from "../Versioned"
import { ExplValueCursor } from "./Cursor"
import { markerEnsureDefined, svgRootElement } from "./Renderer"
import { View } from "./View"
import "./styles.css"

// Previously Editor, but clashes a bit with the Wrattler class of the same name.
export module Pane {
   export function initialise (): void {
      View.initialise()
   }

   export interface Listener {
      onBwdSlice (editor: Pane, externDeps: Slice): void
   }

   export class Pane {
      listener: Listener
      rootPane: SVGSVGElement
      tooltips: Set<Tooltip>
      tooltipPlacement: Placement // make for nicer examples
      ρ_external: Env
      ρ_imports: Env
      e: Expr
      tv: ExplValue
      here!: ExplValueCursor
      direction: Direction
      slice: Slice = new Set()
   
      constructor (
         listener: Listener, 
         [width, height]: [number, number], 
         tooltipPlacement: Placement, 
         ρ_external: Env, 
         ρ_imports: Env,
         e: Expr
      ) {
         this.listener = listener
         this.rootPane = svgRootElement(width, height)
         this.tooltips = new Set()
         this.tooltipPlacement = tooltipPlacement
         markerEnsureDefined(this.rootPane, Arrowhead, "blue")
         document.body.appendChild(this.rootPane)
         this.ρ_external = ρ_external
         this.ρ_imports = ρ_imports
         this.e = e
         // evaluate twice so we can start with an empty delta
         this.tv = Eval.eval_(this.ρ, this.e)
         this.here = ExplValueCursor.descendant(null, this.tv)
         this.direction = Direction.Fwd
         newRevision()
         Eval.eval_(this.ρ, this.e) // reestablish reachable nodes
      }

      get ρ (): Env {
         return this.ρ_external.concat(this.ρ_imports)
      }

      visibleTooltips (): Tooltip[] {
         return [...this.tooltips].filter(tooltip => tooltip.state.isVisible)
      }

      initialise (): void {
         this.render()
         const this_: this = this
         // https://stackoverflow.com/questions/5597060
            document.onkeydown = function (ev: KeyboardEvent) {
            if (ev.shiftKey) {
               if (ev.keyCode == 37) { // left
                  this_.here = this_.here.prevSibling()
                  this_.render()
               } else
               if (ev.keyCode == 38) { // up
                  if (this_.here.hasParent()) {
                     this_.here = this_.here.up()
                     this_.render()
                  }
               } else
               if (ev.keyCode == 39) { // right
                  this_.here = this_.here.nextSibling()
                  this_.render()
               } else
               if (ev.keyCode == 40) { // down
                  if (this_.here.tv.v instanceof DataValue) {
                     this_.here = this_.here.toChild(0)
                     this_.render()
                  }
               }
            }
         }
         document.onkeypress = function (ev: KeyboardEvent) {
            if (ev.shiftKey) {
               if (ev.key === "V") {
                  View.existingView(this_.here.tv).toggleValue()
                  this_.render()
               } else
               if (ev.key === "E") {
                  View.existingView(this_.here.tv).toggleExpl()
                  this_.render()
               }
            }
         }
      }

      bwdSlice (setNeeded: () => void): void {
         __slice.reset(Direction.Bwd)
         setNeeded()
         Eval.eval_bwd(this.e, this.tv)
         __slice.ann = __slice.restrictTo(this.ρ_external.values())
         this.listener.onBwdSlice(this, __slice.ann)
         this.direction = Direction.Bwd
         this.slice = __slice.ann
      }

      // Forward-slice with respect to supplied slice of ρ_external.
      fwdSlice (externDeps: Slice): void {
         __slice.direction = Direction.Fwd
         __slice.ann = externDeps
         Eval.eval_fwd(this.e, this.tv)
         __slice.ann = __slice.restrictTo([this.tv])
         this.direction = Direction.Fwd
         this.slice = __slice.ann
      }

      render (): void {
         // https://stackoverflow.com/questions/48310643
         const children: ChildNode[] = Array.from(this.rootPane.childNodes)
         children.forEach((child: ChildNode): void => {
            if (!(child instanceof SVGDefsElement)) {
               this.rootPane.removeChild(child)
            }
         })
         this.tooltips.forEach(tooltip => tooltip.destroy())
         this.tooltips.clear()
         View.render(this)
      }
   
      onEdit (): void {
         this.tv = Eval.eval_(this.ρ, this.e)
         this.here = ExplValueCursor.descendant(null, explValue(as(this.tv.t, Expl.Defs).t, this.tv.v))
         // cursor may no longer be valid, how to deal with that?
         this.render()
      }
   
      onViewChange (): void {
         this.render()
      }
   }
}
