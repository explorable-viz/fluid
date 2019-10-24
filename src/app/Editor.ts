import { as } from "../util/Core"
import { DataValue, ExplValue, explValue } from "../DataValue"
import { __deltas } from "../Delta"
import { Env, emptyEnv } from "../Env"
import { Eval } from "../Eval"
import { Expl } from "../Expl"
import { Expr } from "../Expr"
import { newRevision } from "../Versioned"
import { ExplValueCursor } from "./Cursor"
import { svg } from "./Renderer"
import { Viewer, existingView } from "./View"
import "./styles.css"

export class Editor {
   root: SVGSVGElement
   ρ: Env
   e: Expr
   tv: ExplValue
   here!: ExplValueCursor

   constructor (e: Expr, ρ: Env = emptyEnv()) {
      this.root = svg.createSvg(1400, 600)
      document.body.appendChild(this.root)
      this.ρ = ρ
      this.e = e,
      this.tv = Eval.eval_(ρ, this.e)
      this.here = ExplValueCursor.descendant(null, explValue(as(this.tv.t, Expl.Defs).t, this.tv.v)) // skip prelude
      newRevision()
      Eval.eval_(ρ, this.e) // reestablish reachable nodes
      // Wait for fonts to load before rendering, otherwise metrics will be wrong.
      window.onload = (ev: Event): void => {
         this.render()
      }
   }

   render (): void {
      // not sure why this shenanigan to clear view
      while (this.root.firstChild !== null) {
         this.root.removeChild(this.root.firstChild)
      }
      const tv: ExplValue = explValue(as(this.tv.t, Expl.Defs).t, this.tv.v) // skip prelude
      const [g,]: [SVGElement, number] = new Viewer().render(tv, this)
      this.root.appendChild(g)
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
               existingView(this_.here.tv).toggleValue()
               this_.render()
            } else
            if (ev.key === "E") {
               existingView(this_.here.tv).toggleExpl()
               this_.render()
            }
         }
      }
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
