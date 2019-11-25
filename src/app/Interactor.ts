import tippy from "tippy.js"
import { Instance as Tooltip } from "tippy.js"
import "tippy.js/dist/tippy.css"
import "tippy.js/themes/light-border.css"
import { __log, __nonNull, as, assert } from "../util/Core"
import { bool_ } from "../util/Lattice"
import { Annotated, Direction, isα, setα } from "../Annotation"
import { ExplValue } from "../DataValue"
import { Expl } from "../Expl"
import { GraphicsElement, Rect } from "../Graphics2"
import { Num, Persistent, Str, fields } from "../Value"
import { ExplValueCursor } from "./Cursor"
import { Editor } from "./Editor"
import { round } from "./Renderer"

function createTooltip (element: SVGElement): Tooltip {
   return tippy(element, { theme: "light-border", placement: "right" })
}

// Non-primitive dependencies render as a bullet.
function propValues<T extends GraphicsElement> (g: T, props: (keyof T)[]): string {
   const lines: string[] = props.map((prop: keyof T): string => {
      const propVal: Persistent = g.__child(prop)
      const propStr: string = propVal instanceof Num ? 
            round(propVal.val) :
            propVal instanceof Str ? 
               propVal.val : "•"
      return `${prop}: ${propStr}`
   })
   return lines.join("</br>")
}

export class RectInteractor {
   editor: Editor.Editor
   tooltip: Tooltip
   tg: ExplValueCursor/*<Rect>*/
   r: SVGRectElement
   propFocus: keyof Rect | null = null

   constructor (editor: Editor.Editor, tg: ExplValueCursor/*<Rect>*/, r: SVGRectElement) {
      this.editor = __nonNull(editor)
      this.tooltip = createTooltip(r)
      this.editor.tooltips.add(this.tooltip)
      this.tg = tg
      this.r = r
      const g: Rect = as(tg.tv.v, Rect)
      const propsFocus: (keyof Rect)[] = fields(tg.tv.v).filter((prop: keyof Rect) => {
         const tv: ExplValue = Expl.explChild(tg.tv.t, g, prop)
         return __nonNull(editor.direction) === Direction.Fwd ? bool_.negate(isα(tv.t)) : isα(tv.t)
      })
      if (propsFocus.length > 0) {
         this.tooltip.setContent(propValues(g, propsFocus))
         this.tooltip.show()
      }
   }

   onMouseMove (e: MouseEvent): void {
      const g: Rect = as(this.tg.tv.v, Rect)
      const rect: ClientRect = this.r.getBoundingClientRect()
      // invert sign on y axis because of global inversion for SVG graphics
      const x_prop: number = Math.max(e.clientX - rect.left, 0) / rect.width
      const y_prop: number = Math.min(rect.bottom - e.clientY, rect.height) / rect.height
      const propFocus: keyof Rect = this.propFor(x_prop, y_prop, 1, 1)
      if (this.propFocus !== propFocus) {
         this.propFocus = propFocus
         const dependencies: Set<Annotated> = this.editor.bwdSlice(() => {
            setα(bool_.top, this.tg.to(Rect, propFocus).tv.t)
         })
         console.log(dependencies)
         this.tooltip.setContent(propValues(g, [propFocus]))
      }
   }

   onMouseOut (e: MouseEvent): void {
      this.propFocus = null
      assert(this.editor.bwdSlice(() => {}).size === 0)
   }

   // Determine which "diagonal quadrant" of the rectangle [width, height] contains [x, y], and
   // then map to the corresponding attribute of the rectangle.
   propFor (x: number, y: number, width: number, height: number): keyof Rect {
      const y_diag: number = (height / width) * x
      const corner: [keyof Rect, keyof Rect] = y > y_diag ? ["x", "height"] : ["y", "width"]
      return y < height - y_diag ? corner[0] : corner[1]
   }
}
