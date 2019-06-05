import { ann } from "../util/Annotated"
import { __nonNull, absurd, assert, className } from "../util/Core"
import { Cons, List } from "../BaseTypes"
import { Direction } from "../Eval"
import { Graphic, GraphicsElement, LinearTransform, Polygon, Polyline, Point, Scale, Text, Transform, Translate, Transpose } from "../Graphics"
import { asVersioned, setallα } from "../Versioned"

export const svgNS: "http://www.w3.org/2000/svg" = "http://www.w3.org/2000/svg"
type TransformFun = (p: [number, number]) => [number, number]

// No counterpart of this in the graphics DSL yet.
export const reflect_y: TransformFun =
   ([x, y]): [number, number] => {
      return [x, -y]
   }

function scale (x_scale: number, y_scale: number): TransformFun {
   return ([x, y]): [number, number] => {
      return [x * x_scale, y * y_scale]
   }
}

function translate (x_inc: number, y_inc: number): TransformFun {
   return ([x, y]): [number, number] => {
      return [x + x_inc, y + y_inc]
   }
}

function postcompose (f1: TransformFun, f2: TransformFun): TransformFun {
   return ([x, y]): [number, number] => {
      return f1(f2([x, y]))
   }
}

const transpose: TransformFun =
   ([x, y]): [number, number] => {
      return [y, x]
   }

export interface Slicer {
   resetForFwd (): void // set all annotations to top
   fwdSlice (): void
   resetForBwd (): void // set all annotations to bot
   bwdSlice (): void    // bwd slice and set polarity to bwd
   direction: Direction
}

export class GraphicsRenderer {
   transforms: TransformFun[] // stack of successive compositions of linear transformations
   ancestors: SVGGraphicsElement[] // stack of enclosing SVG elements
   slicer: Slicer

   constructor (root: SVGSVGElement, slicer: Slicer) {
      this.ancestors = [root]
      this.slicer = slicer
      this.transforms = [x => x]
   }

   get current (): SVGGraphicsElement {
      return this.ancestors[this.ancestors.length - 1]
   }

   get transform (): TransformFun {
      assert(this.transforms.length > 0)
      // query the current transform rather than returing a closure that accesses it...
      const transform: TransformFun = this.transforms[this.transforms.length - 1]
      return ([x, y]) => {
         const [xʹ, yʹ] = transform([x, y])
         return [Math.round(xʹ), Math.round(yʹ)]
      } 
   }

   render (g: GraphicsElement): void {
      assert(this.ancestors.length === 1)
      while (this.current.firstChild !== null) {
         this.current.removeChild(this.current.firstChild)
      }
      this.renderElement(g)
   }

   renderElement (g: GraphicsElement): void {
      if (g instanceof Graphic) {
         this.group(g)
      } else 
      if (g instanceof Polyline) {
         this.polyline(g.points)
      } else
      if (g instanceof Polygon) {
         this.polygon(g.points)
      } else
      if (g instanceof Text) {
         this.text(g)
      } else
      if (g instanceof Transform) {
         const t: LinearTransform = g.t
         if (t instanceof Scale) {
            this.renderWith(g.g, scale(t.x.val, t.y.val))
         } else
         if (t instanceof Translate) {
            this.renderWith(g.g, translate(t.x.val, t.y.val))
         } else
         if (t instanceof Transpose) {
            this.renderWith(g.g, transpose)
         } else {
            return absurd()
         }
      }
      else {
         return absurd()
      }
   }

   group (g: Graphic): void {
      const group: SVGGElement = document.createElementNS(svgNS, "g")
      // See https://www.smashingmagazine.com/2018/05/svg-interaction-pointer-events-property/.
      group.setAttribute("pointer-events", "bounding-box")
      this.current.appendChild(group)
      this.ancestors.push(group)
      for (let gs: List<GraphicsElement> = g.gs; Cons.is(gs); gs = gs.tail) {
         this.renderElement(gs.head)
      }
      group.addEventListener("click", (e: MouseEvent): void => {
         e.stopPropagation()
         this.slicer.resetForBwd()
         console.log(`Setting all annotations on ${className(g)}`)
         setallα(ann.top, g)
         this.slicer.bwdSlice()
      })
      this.ancestors.pop()
   }

   renderWith (g: GraphicsElement, f: TransformFun): void {
      const transform: TransformFun = this.transform
      this.transforms.push(postcompose(transform, f))
      this.renderElement(g)
      this.transforms.pop()
   }

   svgPath (p̅: List<Point>): [number, number][] {
      return p̅.toArray().map(({ x, y }): [number, number] => this.transform([x.val, y.val]))
   }

   points (p̅: List<Point>): string {
      return this.svgPath(p̅).map(([x, y]: [number, number]) => `${x},${y}`).join(" ")
   }

   polyline (p̅: List<Point>): void {
      const path: SVGPolylineElement = document.createElementNS(svgNS, "polyline")
      path.setAttribute("points", this.points(p̅))
      path.setAttribute("stroke", "black")
      this.current.appendChild(path)
      this.pointHighlights(p̅)
   }

   pointHighlights (p̅: List<Point>): void {
      for (; Cons.is(p̅); p̅ = p̅.tail) {
         const p: Point = p̅.head,
               [x, y]: [number, number] = this.transform([p.x.val, p.y.val]),
               [x_α, y_α] = [__nonNull(asVersioned(p.x).__α), __nonNull(asVersioned(p.y).__α)]
         // In the fwd direction, a point appears "erased" (false) if either of its components is erased.
         // In the bwd direction, a point appears "needed" (true) if either of its components is needed.
         if (this.slicer.direction === Direction.Fwd ? ann.join(!x_α, !y_α) : ann.join(x_α, y_α)) {
            this.circle(x, y, 3)
         }
      }
   }

   circle (x: number, y: number, radius: number): void {
      const circle: SVGCircleElement = document.createElementNS(svgNS, "circle")
      circle.setAttribute("cx", x.toString())
      circle.setAttribute("cy", y.toString())
      circle.setAttribute("r", radius.toString())
      circle.setAttribute("stroke", "#0000ff")
      circle.setAttribute("fill", "none")
      this.current.appendChild(circle)
   }

   polygon (p̅: List<Point>): void {
      const polygon: SVGPolygonElement = document.createElementNS(svgNS, "polygon")
      polygon.setAttribute("points", this.points(p̅))
      polygon.setAttribute("stroke", "black")
      polygon.setAttribute("fill", "#f6831e")
      polygon.addEventListener("click", (e: MouseEvent): void => {
         e.stopPropagation()
         this.slicer.resetForBwd()
         p̅.toArray().map((p: Point): void => {
            console.log(`Setting all annotations on ${p}`)
            setallα(ann.top, p)
         })
         this.slicer.bwdSlice()
      })
      this.current.appendChild(polygon)
      this.pointHighlights(p̅)
   }

   // Flip text vertically to cancel out the global vertical flip. Don't set x and y but express
   // position through a translation so that the scaling doesn't affect the position.
   text (g: Text): void {
      const text: SVGTextElement = document.createElementNS(svgNS, "text"),
            [x, y]: [number, number] = this.transform([g.x.val, g.y.val])
      text.setAttribute("stroke", "none")
      text.setAttribute("fill", "black")
      text.setAttribute("font-size", "10")
      text.setAttribute("transform", `translate(${x.toString()},${y.toString()}) scale(1,-1)`)
      text.appendChild(document.createTextNode(g.str.val))
      this.current.appendChild(text)
   }
}
