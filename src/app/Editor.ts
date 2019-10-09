import "../../src/BaseTypes" // otherwise mysterious cyclic initialisation error
import { Expr } from "../Expr"
import { openWithImports } from "../Module"
import { createSvg, svgNS, textElement } from "./Core"
import "./styles.css"

function render (e: Expr): SVGElement {
   if (e instanceof Expr.App) {

   }

   const text: SVGTextElement = textElement(50, 50, 18, "An expression")
   text.setAttribute("class", "code")
   return text
}

class Editor {
   constructor () {
      const root: SVGSVGElement = createSvg(400, 400, false),
            polygon: SVGPolygonElement = document.createElementNS(svgNS, "polygon")
      polygon.setAttribute("points", "0, 0 0, 100 100, 0, 100, 100")
      polygon.setAttribute("stroke", "black")
      polygon.setAttribute("fill", "gray")
      root.appendChild(polygon)
      document.body.appendChild(root)
      const e: Expr = openWithImports("foldr_sumSquares")
      root.appendChild(render(e))
   }
}

new Editor()
