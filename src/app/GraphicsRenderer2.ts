import { __nonNull, absurd, as, assert } from "../util/Core"
import { Cons, List } from "../BaseTypes"
import { ExplValue } from "../DataValue"
import { Graphic, GraphicsElement, Point, Rect } from "../Graphics2"
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

export const svg: SVG = new SVG(true)

type TransformFun = (p: [number, number]) => [number, number]

function scale (x_scale: number, y_scale: number): TransformFun {
   return ([x, y]): [number, number] => {
      return [x * x_scale, y * y_scale]
   }
}

function postcompose (f1: TransformFun, f2: TransformFun): TransformFun {
   return ([x, y]): [number, number] => {
      return f1(f2([x, y]))
   }
}

export class GraphicsRenderer {
   scalings: TransformFun[] // stack of successive compositions of scaling transformations
   ancestors: SVGSVGElement[] // stack of enclosing SVG elements

   constructor (root: SVGSVGElement) {
      this.ancestors = [root]
      this.scalings = [x => x]
   }

   get current (): SVGSVGElement {
      return this.ancestors[this.ancestors.length - 1]
   }

   get transform (): TransformFun {
      assert(this.scalings.length > 0)
      // query the current transform rather than returing a closure that accesses it...
      const transform: TransformFun = this.scalings[this.scalings.length - 1]
      return ([x, y]) => {
         const [xʹ, yʹ] = transform([x, y])
         return [Math.round(xʹ), Math.round(yʹ)]
      } 
   }

   render (tg: ExplValue<GraphicsElement>): void {
      assert(this.ancestors.length === 1)
      const root: SVGSVGElement = this.current
      while (root.firstChild !== null) {
         root.removeChild(root.firstChild)
      }
      const width: number = parseFloat(root.getAttribute("width")!)
      const height: number = parseFloat(root.getAttribute("height")!)
      const transform: TransformFun = this.transform
      this.scalings.push(postcompose(transform, scale(width / tg.v.width.val, height / tg.v.height.val)))
      this.renderElement(ExplValueCursor.descendant(null, tg))
      this.scalings.pop()
   }

   renderElement (tg: ExplValueCursor/*<GraphicsElement>*/): void {
      const g: GraphicsElement = as(tg.tv.v, GraphicsElement)
      const transform: TransformFun = this.transform
      this.scalings.push(postcompose(transform, scale(g.scale.x.val, g.scale.y.val)))
      if (g instanceof Graphic) {
         this.graphic(tg)
      } else 
      if (g instanceof Rect) {
         this.rect(tg)
      } else {
         return absurd()
      }
      this.scalings.pop()
   }

   graphic (tg: ExplValueCursor/*<Graphic>*/): void {
      const svg: SVGSVGElement = document.createElementNS(SVG.NS, "svg")
      this.current.appendChild(svg)
      this.ancestors.push(svg)
      // ignoring annotations on cons cells
      for (let tg̅: ExplValueCursor/*<List<GraphicsElement>>*/ = tg.to(Graphic, "gs"); 
           Cons.is(as(tg̅.tv.v, List)); tg̅ = tg̅.to(Cons, "tail")) {
         this.renderElement(tg̅.to(Cons, "head"))
      }
      this.ancestors.pop()
   }

   transformedPath (p̅: List<Point>): [number, number][] {
      return p̅.toArray().map(({ x, y }): [number, number] => this.transform([x.val, y.val]))
   }

   asString (p̅: [number, number][]): string {
      return p̅.map(([x, y]: [number, number]) => `${x},${y}`).join(" ")
   }

   rect (tg: ExplValueCursor/*<Rect>*/): void {
      const rect: SVGRectElement = document.createElementNS(SVG.NS, "rect"),
            g: Rect = as(tg.tv.v, Rect)
      rect.setAttribute("x", `${g.x.val}`)
      rect.setAttribute("y", `${g.y.val}`)
      rect.setAttribute("width", `${g.width.val}`)
      rect.setAttribute("height", `${g.height.val}`)
      rect.setAttribute("stroke", g.stroke.val)
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
