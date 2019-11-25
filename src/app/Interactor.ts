import tippy from "tippy.js"
import { Instance as Tooltip, Placement } from "tippy.js"
import "tippy.js/dist/tippy.css"
import "tippy.js/themes/light-border.css"
import { Class, __log, __nonNull, as, assert } from "../util/Core"
import { bool_ } from "../util/Lattice"
import { Direction, isα, setα } from "../Annotation"
import { DataValue, ExplValue } from "../DataValue"
import { Expl } from "../Expl"
import { Point, Rect } from "../Graphics2"
import { Num, Persistent, Str, fields } from "../Value"
import { ExplValueCursor } from "./Cursor"
import { Editor } from "./Editor"
import { round } from "./Renderer"

function createTooltip (element: SVGElement, placement: Placement): Tooltip {
   return tippy(element, { theme: "light-border", placement })
}

abstract class Interactor<T extends DataValue, U extends SVGElement> {
   editor: Editor.Editor
   C: Class<T>
   tooltip: Tooltip
   cursor: ExplValueCursor/*<Pair<Num, Num>>*/
   element: U
   propFocus: keyof T | null = null

   constructor (editor: Editor.Editor, C: Class<T>, cursor: ExplValueCursor/*<Pair<Num, Num>>*/, element: U) {
      this.editor = editor
      this.C = C
      this.tooltip = createTooltip(element, editor.tooltipPlacement)
      this.editor.tooltips.add(this.tooltip)
      this.cursor = cursor
      this.element = element
      const p: T = as(cursor.tv.v, C)
      const propsFocus: (keyof T)[] = this.focusedProps(cursor.tv as ExplValue<T>)
      if (propsFocus.length > 0) {
         this.tooltip.setContent(this.propValues(p, propsFocus))
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

   // Non-primitive dependencies render as a bullet.
   propValues (g: T, props: (keyof T)[]): string {
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

   focusedProps<T extends DataValue> (tv: ExplValue<T>): (keyof T)[] {
      return fields(tv.v).filter((prop: keyof T) => {
         const tv_: ExplValue = Expl.explChild(tv.t, tv.v, prop)
         return __nonNull(this.editor.direction) === Direction.Fwd ? bool_.negate(isα(tv_)) : isα(tv_)
      })
   }

   abstract propFor (x_prop: number, y_prop: number): keyof T

   onMouseMove (e: MouseEvent): void {
      const v: T = as(this.cursor.tv.v, this.C)
      const rect: ClientRect = this.element.getBoundingClientRect()
      // invert sign on y axis because of global inversion for SVG graphics
      const x_prop: number = Math.max(e.clientX - rect.left, 0) / rect.width
      const y_prop: number = Math.min(rect.bottom - e.clientY, rect.height) / rect.height
      const propFocus: keyof T = this.propFor(x_prop, y_prop)
      if (this.propFocus !== propFocus) {
         this.propFocus = propFocus
         this.editor.bwdSlice(() => {
            setα(bool_.top, this.cursor.to(this.C, propFocus).tv)
         })
         this.tooltip.setContent(this.propValues(v, [propFocus]))
         this.element.classList.add("focus")
      }
   }

   onMouseOut (e: MouseEvent): void {
      this.propFocus = null
      assert(this.editor.bwdSlice(() => {}).size === 0)
      this.element.classList.remove("focus")
   }
}

export class PointInteractor extends Interactor<Point, SVGElement> {
   constructor (editor: Editor.Editor, tp: ExplValueCursor/*<Point>*/, marker: SVGElement) {
      super(editor, Point, tp, marker)
   }

   propFor (x_prop: number, y_prop: number): keyof Point {
      return "y"
   }
}

export class RectInteractor extends Interactor<Rect, SVGRectElement> {
   constructor (editor: Editor.Editor, tg: ExplValueCursor/*<Rect>*/, r: SVGRectElement) {
      super(editor, Rect, tg, r)
   }

   // Determine which "diagonal quadrant" of the unit square [1, 1] contains [x, y], and
   // then map to the corresponding attribute of the rectangle.
   propFor (x: number, y: number): keyof Rect {
      const corner: [keyof Rect, keyof Rect] = y > x ? ["x", "height"] : ["y", "width"]
      return y < 1 - x ? corner[0] : corner[1]
   }
}
