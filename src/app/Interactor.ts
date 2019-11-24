import tippy from "tippy.js"
import { Instance } from "tippy.js"
import "tippy.js/dist/tippy.css"
import "tippy.js/themes/light-border.css"
import { __log, __nonNull, as } from "../util/Core"
import { bool_ } from "../util/Lattice"
import { setα } from "../Annotated"
import { Rect } from "../Graphics2"
import { Num } from "../Value"
import { ExplValueCursor } from "./Cursor"
import { Editor } from "./Editor"
import { round } from "./Renderer"

export class Interactor {
   editor: Editor.Editor
   tooltip: Instance | null = null // just have one for now
   tooltips: Map<SVGElement, Instance> = new Map()

   constructor (editor: Editor.Editor) {
      this.editor = __nonNull(editor)
   }

   initialiseElement (element: SVGElement): void {
      this.tooltips.set(element, tippy(element, { theme: "light-border" }))
   }

   onRectMousemove (tg: ExplValueCursor/*<Rect>*/, r: SVGRectElement, e: MouseEvent): void {
      const g: Rect = as(tg.tv.v, Rect)
      const rect: ClientRect = r.getBoundingClientRect()
      // invert sign on y axis because of global inversion for SVG graphics
      const x_prop: number = Math.max(e.clientX - rect.left, 0) / rect.width
      const y_prop: number = Math.min(rect.bottom - e.clientY, rect.height) / rect.height
      const prop: keyof Rect = rectAttribute(x_prop, y_prop, 1, 1)
      const content: string = `${prop}: ${round(as(g[prop], Num).val)}`
      const tooltip: Instance = __nonNull(this.tooltips.get(r))
      tooltip.setContent(content)

      this.editor.resetForBwd()
      setα(bool_.top, tg.to(Rect, prop).tv.t)
      this.editor.bwdSlice()
}

   onRectMouseOut (): void {
      __nonNull(this.tooltip).hide()
   }
}

// Determine which "diagonal quadrant" of the rectangle [width, height] contains [x, y], and
// then map to the corresponding attribute of the rectangle.
function rectAttribute (x: number, y: number, width: number, height: number): keyof Rect {
   const y_diag: number = (height / width) * x
   const corner: [keyof Rect, keyof Rect] = y > y_diag ? ["x", "height"] : ["y", "width"]
   return y < height - y_diag ? corner[0] : corner[1]
}
