import tippy from "tippy.js"
import { Instance } from "tippy.js"
import "tippy.js/dist/tippy.css"
import "tippy.js/themes/light-border.css"
import { __log, __nonNull } from "../util/Core"
import { Rect } from "../Graphics2"

export class Interactor {
   tooltip: Instance | null = null // just have one for now
   tooltips: Map<SVGElement, Instance> = new Map()

   initialise (element: SVGElement): void {
      this.tooltips.set(element, tippy(element, { theme: "light-border" }))
   }

   onRectMousemove (g: Rect, r: SVGRectElement, e: MouseEvent): void {
      const rect: ClientRect = r.getBoundingClientRect()
      // invert sign on y axis because of global inversion for SVG graphics
      const x_prop: number = Math.max(e.clientX - rect.left, 0) / rect.width
      const y_prop: number = Math.min(rect.bottom - e.clientY, rect.height) / rect.height
      const prop: keyof Rect = rectAttribute(x_prop, y_prop, 1, 1)
      const content: string = `${prop}: ${g[prop]}`
      const tooltip: Instance = __nonNull(this.tooltips.get(r))
      tooltip.setContent(content)
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
