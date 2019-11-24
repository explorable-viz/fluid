import { __nonNull, as } from "../util/Core"
import { Direction } from "../Annotated"
import { DataValue, ExplValue, explValue } from "../DataValue"
import { __deltas } from "../Delta"
import { Env } from "../Env"
import { Eval } from "../Eval"
import { Expl } from "../Expl"
import { Expr } from "../Expr"
import { Arrowhead } from "../Graphics2"
import { ExplValueCursor } from "./Cursor"
import { markerEnsureDefined, svgRootElement } from "./Renderer"
import { View } from "./View"
import "./styles.css"

export module Editor {
   export function initialise (): void {
      View.initialise()
   }

   export class Editor {
      root: SVGSVGElement
      ρ: Env
      e: Expr
      tv: ExplValue
      here!: ExplValueCursor
      direction!: Direction
   
      constructor (ρ_external: Env, ρ: Env, e: Expr) {
         this.root = svgRootElement(1400, 1200)
         markerEnsureDefined(this.root, Arrowhead, "blue")
         document.body.appendChild(this.root)
         this.ρ = ρ_external.concat(ρ)
         this.e = e
         this.tv = Eval.eval_(ρ, this.e)
         this.here = ExplValueCursor.descendant(null, this.tv)
         // Wait for fonts to load before rendering, otherwise metrics will be wrong.
         window.onload = (ev: Event) => this.onload(ev)
      }

      onload (ev: Event): void {
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

      // Consider availability of ρ_external only; treat ρ and e as unlimited resources.
      fwdSlice (): void {
         Eval.eval_fwd(this.e, this.tv)
         this.direction = Direction.Fwd
         this.render()
      }
  
      render (): void {
         // https://stackoverflow.com/questions/48310643
         const children: ChildNode[] = Array.from(this.root.childNodes)
         children.forEach((child: ChildNode): void => {
            if (!(child instanceof SVGDefsElement)) {
               this.root.removeChild(child)
            }
         })
         View.render(this.root, this.tv, this)
      }
   
      onEdit (): void {
         this.tv = Eval.eval_(this.ρ, this.e)
         this.here = ExplValueCursor.descendant(null, explValue(as(this.tv.t, Expl.Defs).t, this.tv.v)) // skip prelude
         // cursor may no longer be valid, how to deal with that?
         this.render()
      }
   
      onViewChange (): void {
         this.render()
      }
   }
}
