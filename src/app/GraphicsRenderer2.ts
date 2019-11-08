import { __nonNull, absurd, as, assert } from "../util/Core"
import { Cons, List } from "../BaseTypes"
import { ExplValue } from "../DataValue"
import { Graphic, GraphicsElement, Rect } from "../Graphics2"
import { Unary, unary_, unaryOps } from "../Primitive"
import { Id, Num, Str } from "../Value"
import { num } from "../Versioned"
import { SVG } from "./Core"
import { ExplValueCursor } from "./Cursor"

const fontSize: number = 12

// The SVG text element for the supplied text; centralised so can be used to compute text metrics.
// Use "translate" to locate the element, so that we can apply it after scaling.
function textElement (x: number, y: number, fontSize: number, str: string): SVGTextElement {
   const text: SVGTextElement = document.createElementNS(SVG.NS, "text")
   text.setAttribute("stroke", "none")
   text.setAttribute("font-size", fontSize.toString())
   let transform: string = `translate(${x.toString()},${y.toString()})`
   text.setAttribute("transform", transform + " scale(1,-1)")
   text.appendChild(document.createTextNode(str))
   return text
}

export const svg: SVG = new SVG()

type ScaleFactor = [number, number]

function scale (x_scale: number, y_scale: number): ScaleFactor {
   return [x_scale, y_scale]
}

function postcompose ([x1, y1]: ScaleFactor, [x2, y2]: ScaleFactor): ScaleFactor {
   return [x1 * x2, y1 * y2]
}

export class GraphicsRenderer {
   scalings: ScaleFactor[] // stack of successive compositions of scaling transformations
   ancestors: SVGElement[] // stack of enclosing SVG elements

   constructor (root: SVGElement) {
      this.ancestors = [root]
      this.scalings = [[1, 1]]
   }

   get current (): SVGElement {
      return this.ancestors[this.ancestors.length - 1]
   }

   get scale (): ScaleFactor {
      assert(this.scalings.length > 0)
      return this.scalings[this.scalings.length - 1]
   }

   render (tg: ExplValue<GraphicsElement>): void {
      assert(this.ancestors.length === 1)
      const root: SVGElement = this.current
      while (root.firstChild !== null) {
         root.removeChild(root.firstChild)
      }
      const width: number = parseFloat(__nonNull(root.getAttribute("width")))
      const height: number = parseFloat(__nonNull(root.getAttribute("height")))
      this.scalings.push(
         postcompose(
            this.scale, 
            scale(width / (tg.v.x.val + tg.v.width.val), height / (tg.v.y.val + tg.v.height.val))
         )
      )
      this.renderElement(ExplValueCursor.descendant(null, tg))
      this.scalings.pop()
   }

   renderElement (tg: ExplValueCursor/*<GraphicsElement>*/): void {
      const g: GraphicsElement = as(tg.tv.v, GraphicsElement)
      if (g instanceof Graphic) {
         this.graphic(tg)
      } else 
      if (g instanceof Rect) {
         this.rect(tg)
      } else {
         return absurd()
      }
   }

   graphic (tg: ExplValueCursor/*<Graphic>*/): void {
      const svg: SVGSVGElement = document.createElementNS(SVG.NS, "svg")
      this.current.appendChild(svg)
      this.ancestors.push(svg)
      for (let tg̅: ExplValueCursor/*<List<GraphicsElement>>*/ = tg.to(Graphic, "gs"); 
           Cons.is(as(tg̅.tv.v, List)); tg̅ = tg̅.to(Cons, "tail")) {
         this.renderElement(tg̅.to(Cons, "head"))
      }
      this.ancestors.pop()
   }

   asString (p̅: [number, number][]): string {
      return p̅.map(([x, y]: [number, number]) => `${x},${y}`).join(" ")
   }

   rect (tg: ExplValueCursor/*<Rect>*/): void {
      const rect: SVGRectElement = document.createElementNS(SVG.NS, "rect"),
            g: Rect = as(tg.tv.v, Rect)
      const [x_scale, y_scale] = this.scale
      const [x, y] = [Math.round(g.x.val * x_scale), Math.round(g.y.val * y_scale)]
      const [width, height] = [Math.round(g.width.val * x_scale), Math.round(g.height.val * y_scale)]
      rect.setAttribute("x", `${x}`)
      rect.setAttribute("y", `${y}`)
      rect.setAttribute("width", `${width}`)
      rect.setAttribute("height", `${height}`)
      rect.setAttribute("fill", g.fill.val)
      this.current.appendChild(rect)
   }
}

{
   // Additional primitives that rely on offline rendering to compute text metrics. Combining these would 
   // require more general primitives that can return tuples.
   const textWidth: Unary<Str, Num> = (str: Str): (k: Id) => Num => {
      return num(svg.textWidth(textElement(0, 0, fontSize, str.val)))
   }
   
   const textHeight: Unary<Str, Num> = (str: Str): (k: Id) => Num => {
      return num(svg.textHeight(textElement(0, 0, fontSize, str.val)))
   }
   
   unaryOps.set(textWidth.name, unary_(textWidth))
   unaryOps.set(textHeight.name, unary_(textHeight))
}
