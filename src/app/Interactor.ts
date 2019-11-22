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
      const x_or_width: string = x_prop >= 0.5 ? `width: ${g.width}` : `x: ${g.x}`
      const y_or_height: string = y_prop >= 0.5 ? `height: ${g.height}` : `y: ${g.y}`
      const content: string = `${x_or_width}<br/>${y_or_height}`
      const tooltip: Instance = __nonNull(this.tooltips.get(r))
      tooltip.setContent(content)
   }

   onRectMouseOut (): void {
      __nonNull(this.tooltip).hide()
   }
}
