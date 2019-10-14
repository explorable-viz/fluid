import { as } from "../util/Core"
import { ExplValue } from "../DataValue"
import { __deltas } from "../Delta"
import { emptyEnv } from "../Env"
import { Eval } from "../Eval"
import { Expr } from "../Expr"
import { openWithImports } from "../Module"
import { ExprCursor } from "./Cursor"
import { Renderer, svg } from "./Renderer"
import "./styles.css"

export class Editor {
   root: SVGSVGElement
   e0: Expr
   e: Expr
   e_cursor: ExprCursor
   tv: ExplValue

   constructor () {
      this.root = svg.createSvg(800, 400)
      document.body.appendChild(this.root)
      this.e0 = openWithImports("pattern-match"),
      this.e = as(this.e0, Expr.Defs).e
      this.e_cursor = new ExprCursor(this.e)
      this.tv = Eval.eval_(emptyEnv(), this.e0) 
      __deltas.clear()         
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
      this.root.appendChild(new Renderer(this).prompt(this.e, this.tv.v))
      document.onkeydown = function(ev: KeyboardEvent) {
         if (ev.keyCode == 40) {
           console.log("Down!")
         }
      }
   }

   onEdit (): void {
      this.tv = Eval.eval_(emptyEnv(), this.e0)
      this.render()
   }
}
