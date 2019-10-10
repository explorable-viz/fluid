import "../../src/BaseTypes" // otherwise mysterious cyclic initialisation error
import { as } from "../util/Core"
import { Expr } from "../Expr"
import { openWithImports } from "../Module"
import { createSvg, svgMetrics, svgNS, textElement, textHeight } from "./Core"
import "./styles.css"

const fontSize: number = 18
const lineHeight: number = Math.ceil(textHeight(fontSize, "m")) // representative character 

// Post-condition: returned element has an entry in "dimensions" map. 
function render (x: number, line: number, e: Expr): SVGElement {
   if (e instanceof Expr.Var) {
      return renderText(x, line, e.x.val)
   } else
   if (e instanceof Expr.App) {
      return renderHoriz(x, line, e.f, e.e)
   } else {
      return renderText(x, line, "TODO")
   }
}

function renderHoriz (x: number, line: number, ...es: Expr[]): SVGElement {
   const x0: number = x,
         g: SVGGElement = document.createElementNS(svgNS, "g")
   let height_max: number = 0
   // See https://www.smashingmagazine.com/2018/05/svg-interaction-pointer-events-property/.
   g.setAttribute("pointer-events", "bounding-box")
   for (const e of es) {
      const v: SVGElement = render(x, line, e),
            { width, height } = dimensions.get(v)!
      x += width
      height_max = Math.max(height_max, height)
      g.appendChild(v)
   }
   dimensions.set(g, { width: x - x0, height: height_max })
   return g
}

function renderText (x: number, line: number, str: string): SVGTextElement {
   const text: SVGTextElement = textElement(x, line * lineHeight, fontSize, str)
//   text.setAttribute("class", "code")
   text.setAttribute("font-family", "Inconsolata")
   svgMetrics.appendChild(text)
   dimensions.set(text, { width: text.getBBox().width, height: lineHeight })
   text.remove()
   return text
}

type Dimensions = { width: number, height: number }

// Populate this explicity, rather than using a memoised function.
const dimensions: Map<SVGElement, Dimensions> = new Map()

class Editor {
   constructor () {
      window.onload = (ev: Event): void => {
         const root: SVGSVGElement = createSvg(400, 400, false),
         polygon: SVGPolygonElement = document.createElementNS(svgNS, "polygon")
         polygon.setAttribute("points", "0, 0 0, 100 100, 0, 100, 100")
         polygon.setAttribute("stroke", "black")
         polygon.setAttribute("fill", "gray")
         root.appendChild(polygon)
         document.body.appendChild(root)
         const e: Expr = as(openWithImports("foldr_sumSquares"), Expr.Defs).e
         root.appendChild(render(0, 0, e))
      }
   }
}

new Editor()

/*
declare class FontLoader {
   constructor (blah1: string[], blah2: any, blah3: any)
}

new FontLoader(["inconsolata"], {
   "fontLoaded": function(font: any) {
      // One of the fonts was loaded
      console.log("font loaded: " + font.family);
   },
   "complete": function(error: any) {
      if (error !== null) {
         // Reached the timeout but not all fonts were loaded
         console.log(error.message)
         console.log(error.notLoadedFonts)
      } else {
         // All fonts were loaded
         console.log("all fonts were loaded")
         new Editor()
      }
   }
}, 3000)
*/
