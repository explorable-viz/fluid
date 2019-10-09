import { openWithImports } from "../Module"
import { createSvg, svgNS } from "./Core"

class Editor {
   constructor () {
      openWithImports("foldr_sumSquares")

      const root: SVGSVGElement = createSvg(400, 400, false),
            polygon: SVGPolygonElement = document.createElementNS(svgNS, "polygon")
      polygon.setAttribute("points", "0, 0 100, 0 0, 100, 100, 100")
      polygon.setAttribute("stroke", "black")
      polygon.setAttribute("fill", "gray")
      root.appendChild(polygon)
      document.body.appendChild(root)
   }
}

new Editor()
