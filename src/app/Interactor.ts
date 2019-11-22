import { __log } from "../util/Core"
import { Rect } from "../Graphics2"

import tippy from 'tippy.js'
import 'tippy.js/dist/tippy.css'

export class Interactor {
   onRectMousemove (g: Rect, r: SVGRectElement, e: MouseEvent) {
      const rect: ClientRect = r.getBoundingClientRect()
      // invert sign on y axis because of global inversion for SVG graphics
      const x_prop: number = Math.max(e.clientX - rect.left, 0) / rect.width
      const y_prop: number = Math.min(rect.bottom - e.clientY, rect.height) / rect.height
      const x_or_width: string = x_prop >= 0.5 ? "width" : "x"
      const y_or_height: string = y_prop >= 0.5 ? "height": "y"
      console.log(`(${x_or_width}, ${y_or_height})`)
      r.setAttribute("data-tippy-content", "Hello")
      tippy(r) 
   }
}
