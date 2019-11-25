import tippy from "tippy.js"
import { Instance as Tooltip, Placement } from "tippy.js"
import "tippy.js/dist/tippy.css"
import "tippy.js/themes/light-border.css"
import { Class, __log, __nonNull, as, assert } from "../util/Core"
import { bool_ } from "../util/Lattice"
import { Direction, isα, setα } from "../Annotation"
import { Pair } from "../BaseTypes"
import { DataValue, ExplValue } from "../DataValue"
import { Expl } from "../Expl"
import { Rect } from "../Graphics2"
import { Num, Persistent, Str, Value, fields } from "../Value"
import { ExplValueCursor } from "./Cursor"
import { Editor } from "./Editor"
import { round } from "./Renderer"

function createTooltip (element: SVGElement, placement: Placement): Tooltip {
   return tippy(element, { theme: "light-border", placement })
}

// Non-primitive dependencies render as a bullet.
function propValues<T extends Value> (g: T, props: (keyof T)[]): string {
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

abstract class Interactor<T, U extends SVGElement> {
   editor: Editor.Editor
   tooltip: Tooltip
   cursor: ExplValueCursor/*<Pair<Num, Num>>*/
   element: U
   propFocus: keyof T | null = null

   constructor (editor: Editor.Editor, C: Class<T>, cursor: ExplValueCursor/*<Pair<Num, Num>>*/, element: U) {
      this.editor = editor
      this.tooltip = createTooltip(element, editor.tooltipPlacement)
      this.editor.tooltips.add(this.tooltip)
      this.cursor = cursor
      this.element = element
      const p: Pair<Num, Num> = as(cursor.tv.v, C)
      const propsFocus: (keyof Pair)[] = this.focusedProps(cursor.tv as ExplValue<Pair>)
      if (propsFocus.length > 0) {
         this.tooltip.setContent(propValues(p, propsFocus))
         this.tooltip.show()
         element.classList.add("focus")
      }
      element.addEventListener("mousemove", (e: MouseEvent): void => {
         e.stopPropagation()
         this.onMouseMove(e)
      })
      element.addEventListener("mouseout", (e: MouseEvent): void => {
         e.stopPropagation()
         this.onMouseOut(e)
      })
   }

   focusedProps<T extends DataValue> (tv: ExplValue<T>): (keyof T)[] {
      return fields(tv.v).filter((prop: keyof T) => {
         const tv_: ExplValue = Expl.explChild(tv.t, tv.v, prop)
         return __nonNull(this.editor.direction) === Direction.Fwd ? bool_.negate(isα(tv_)) : isα(tv_)
      })
   }
   
   abstract onMouseMove (e: MouseEvent): void

   onMouseOut (e: MouseEvent): void {
      this.propFocus = null
      assert(this.editor.bwdSlice(() => {}).size === 0)
      this.element.classList.remove("focus")
   }
}

export class PointInteractor extends Interactor<Pair, SVGElement> {
   constructor (editor: Editor.Editor, tp: ExplValueCursor/*<Pair<Num, Num>>*/, marker: SVGElement) {
      super(editor, Pair, tp, marker)
   }

   onMouseMove (e: MouseEvent): void {
   }
}

export class RectInteractor extends Interactor<Rect, SVGRectElement> {
   constructor (editor: Editor.Editor, tg: ExplValueCursor/*<Rect>*/, r: SVGRectElement) {
      super(editor, Rect, tg, r)
   }

   onMouseMove (e: MouseEvent): void {
      const g: Rect = as(this.cursor.tv.v, Rect)
      const rect: ClientRect = this.element.getBoundingClientRect()
      // invert sign on y axis because of global inversion for SVG graphics
      const x_prop: number = Math.max(e.clientX - rect.left, 0) / rect.width
      const y_prop: number = Math.min(rect.bottom - e.clientY, rect.height) / rect.height
      const propFocus: keyof Rect = this.propFor(x_prop, y_prop, 1, 1)
      if (this.propFocus !== propFocus) {
         this.propFocus = propFocus
         this.editor.bwdSlice(() => {
            setα(bool_.top, this.cursor.to(Rect, propFocus).tv)
         })
         this.tooltip.setContent(propValues(g, [propFocus]))
         this.element.classList.add("focus")
      }
   }

   // Determine which "diagonal quadrant" of the rectangle [width, height] contains [x, y], and
   // then map to the corresponding attribute of the rectangle.
   propFor (x: number, y: number, width: number, height: number): keyof Rect {
      const y_diag: number = (height / width) * x
      const corner: [keyof Rect, keyof Rect] = y > y_diag ? ["x", "height"] : ["y", "width"]
      return y < height - y_diag ? corner[0] : corner[1]
   }
}
