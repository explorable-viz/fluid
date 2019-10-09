import { Expr } from "../Expr"
import { openWithImports } from "../Module"
import { createSvg, svgNS } from "./Core"
import "./styles.css"

function textElement (x: number, y: number, str: string): SVGTextElement {
   const text: SVGTextElement = document.createElementNS(svgNS, "text")
   text.setAttribute("stroke", "none")
   text.setAttribute("font-size", "12")
   text.setAttribute("font-family", "courier")
   text.setAttribute("transform", `translate(${x.toString()},${y.toString()}) scale(1,-1)`)
   text.setAttribute("class", "code")
   text.appendChild(document.createTextNode(str))
   return text
}

function render (e: Expr): SVGElement {
   return textElement(50, 50, "An expression")
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
