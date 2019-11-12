import { __nonNull, absurd, as, assert, error } from "../util/Core"
import { Cons, List, None, Option, Pair, Some } from "../BaseTypes"
import { ExplValue } from "../DataValue"
import { Group, GraphicsElement, Polyline, Rect, Scale } from "../Graphics2"
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

   render (tg: ExplValue<GraphicsElement>, [w, h]: [number, number]): void {
      assert(this.ancestors.length === 1)
      const root: SVGElement = this.current
      while (root.firstChild !== null) {
         root.removeChild(root.firstChild)
      }
      const width: number = parseFloat(__nonNull(root.getAttribute("width")))
      const height: number = parseFloat(__nonNull(root.getAttribute("height")))
      this.scalings.push(postcompose(this.scale, scale(width / w, height / h)))
      this.renderElement(ExplValueCursor.descendant(null, tg))
      this.scalings.pop()
   }

   renderElement (tg: ExplValueCursor/*<GraphicsElement>*/): void {
      const g: GraphicsElement = as(tg.tv.v, GraphicsElement)
      if (g instanceof Group) {
         this.group(tg)
      } else 
      if (g instanceof Rect) {
         this.rect(tg)
      } else
      if (g instanceof Polyline) {
         this.polyline(tg)
      } else {
         return absurd()
      }
   }

   withLocalScale<T> (scale: Option<Scale>, localRender: () => T): T {
      let t: T
      if (scale instanceof Some) {
         if (scale.t instanceof Scale) {
            this.scalings.push(postcompose(this.scale, [scale.t.x.val, scale.t.y.val]))
         } else {
            error(`${scale.t} is not a ${Scale.name}.`)
         }
      } else {
         assert(scale instanceof None)
      }
      t = localRender()
      if (scale instanceof Some) {
         this.scalings.pop()
      }
      return t
   }

   group (tg: ExplValueCursor/*<Graphic>*/): void {
      const svg: SVGSVGElement = document.createElementNS(SVG.NS, "svg")
      const g: Group = as(tg.tv.v, Group)
      const [x, y] = scaleBy(g.x, g.y, this.scale)
      // x and y attributes are relative to parent coordinate space, so not transformed.
      // width and height refer to size of viewport (again in parent coordinate space), although currently
      // we ignore these; we should really clip the child content.
      svg.setAttribute("x", `${x}`)
      svg.setAttribute("y", `${y}`)
      this.current.appendChild(svg)
      this.ancestors.push(svg)
      this.withLocalScale(g.scale, () => {
         for (let tg̅: ExplValueCursor/*<List<GraphicsElement>>*/ = tg.to(Group, "gs"); 
         Cons.is(as(tg̅.tv.v, List)); tg̅ = tg̅.to(Cons, "tail")) {
            this.renderElement(tg̅.to(Cons, "head"))
         }
      })
      this.ancestors.pop()
   }

   rect (tg: ExplValueCursor/*<Rect>*/): void {
      const rect: SVGRectElement = document.createElementNS(SVG.NS, "rect")
      const g: Rect = as(tg.tv.v, Rect)
      const [x, y] = scaleBy(g.x, g.y, this.scale)
      const [width, height] = scaleBy(g.width, g.height, this.scale)
      rect.setAttribute("x", `${x}`)
      rect.setAttribute("y", `${y}`)
      rect.setAttribute("width", `${width}`)
      rect.setAttribute("height", `${height}`)
      rect.setAttribute("fill", g.fill.val)
      this.current.appendChild(rect)
   }

   polyline (tg: ExplValueCursor/*<Polyline>*/): void {
      const path: SVGPolylineElement = document.createElementNS(SVG.NS, "polyline")
      const g: Polyline = as(tg.tv.v, Polyline)
      // each point is considered a "child", and therefore subject to my local scaling
      const ps: [number, number][] = this.withLocalScale(g.scale, () => {
         return g.points.toArray().map((p: Pair<Num, Num>): [number, number] => {
            return scaleBy(p.fst, p.snd, this.scale)
         })
      })
      path.setAttribute("points", asString(ps))
      path.setAttribute("stroke", "black")
      path.setAttribute("fill", "none")
      this.current.appendChild(path)
   }
}

function asString (p̅: [number, number][]): string {
   return p̅.map(([x, y]: [number, number]) => `${x},${y}`).join(" ")
}

function scaleBy (x: Num, y: Num, [x_scale, y_scale]: ScaleFactor): [number, number] {
   return [Math.round(x.val * x_scale), Math.round(y.val * y_scale)]
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
