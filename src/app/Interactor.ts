import tippy from "tippy.js"
import { Instance } from "tippy.js"
import "tippy.js/dist/tippy.css"
import "tippy.js/themes/light-border.css"
import { __log, __nonNull, as } from "../util/Core"
import { bool_ } from "../util/Lattice"
import { Direction, isα, setα } from "../Annotation"
import { ExplValue } from "../DataValue"
import { Expl } from "../Expl"
import { Rect } from "../Graphics2"
import { Num, fields } from "../Value"
import { ExplValueCursor } from "./Cursor"
import { Editor } from "./Editor"
import { round } from "./Renderer"

function createTooltip (element: SVGElement): Instance {
   return tippy(element, { theme: "light-border" })
}

export class RectInteractor {
   editor: Editor.Editor
   tooltip: Instance
   tg: ExplValueCursor/*<Rect>*/
   r: SVGRectElement
   propFocus: keyof Rect | null = null

   constructor (editor: Editor.Editor, tg: ExplValueCursor/*<Rect>*/, r: SVGRectElement) {
      this.editor = __nonNull(editor)
      this.tooltip = createTooltip(r)
      this.tg = tg
      this.r = r
      const propsFocus: (keyof Rect)[] = fields(tg.tv.v).filter((prop: keyof Rect) => {
         const tv: ExplValue = Expl.explChild(tg.tv.t, as(tg.tv.v, Rect), prop)
         return editor.direction === Direction.Fwd ? bool_.negate(isα(tv)) : isα(tv)
      })
      if (propsFocus.length > 0) {
         console.log("***** " + propsFocus)
      }
   }

   onMousemove (e: MouseEvent): void {
      const g: Rect = as(this.tg.tv.v, Rect)
      const rect: ClientRect = this.r.getBoundingClientRect()
      // invert sign on y axis because of global inversion for SVG graphics
      const x_prop: number = Math.max(e.clientX - rect.left, 0) / rect.width
      const y_prop: number = Math.min(rect.bottom - e.clientY, rect.height) / rect.height
      const propFocus: keyof Rect = this.propFor(x_prop, y_prop, 1, 1)
      if (this.propFocus !== propFocus) {
         this.propFocus = propFocus
         const content: string = `${this.propFocus}: ${round(as(g[this.propFocus], Num).val)}`
         this.tooltip.setContent(content)
         this.editor.resetForBwd()
         setα(bool_.top, this.tg.to(Rect, this.propFocus).tv.t)
         this.editor.bwdSlice()
      }
   }

   onMouseOut (): void {
      __nonNull(this.tooltip).hide()
   }

   // Determine which "diagonal quadrant" of the rectangle [width, height] contains [x, y], and
   // then map to the corresponding attribute of the rectangle.
   propFor (x: number, y: number, width: number, height: number): keyof Rect {
      const y_diag: number = (height / width) * x
      const corner: [keyof Rect, keyof Rect] = y > y_diag ? ["x", "height"] : ["y", "width"]
      return y < height - y_diag ? corner[0] : corner[1]
   }
}
