import { __nonNull, absurd, assert } from "../util/Core"
import { Cons, List } from "../BaseTypes"
import { Graphic, GraphicsElement, LinearTransform, PathStroke, Point, RectFill, Scale, Transform, Translate, Transpose } from "../Graphics"
import { asVersioned } from "../Versioned"

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

export class GraphicsRenderer {
   transforms: TransformFun[] // stack of successive compositions of linear transformations
   canvas: HTMLCanvasElement
   ctx: CanvasRenderingContext2D
   svg: SVGSVGElement

   constructor (canvas: HTMLCanvasElement, svg: SVGSVGElement) {
      this.canvas = canvas
      this.ctx = __nonNull(canvas.getContext("2d"))
      this.svg = svg
      // convert to a bottom-left frame of reference
      this.transforms = [x => x] // [postcompose(translate(0, canvas.height), reflect_y)]
   }

   get transform (): TransformFun {
      assert(this.transforms.length > 0)
      return this.transforms[this.transforms.length - 1]
   }

   render (g: GraphicsElement): void {
      this.ctx.fillStyle = "white"
		this.ctx.fillRect(0, 0, this.canvas.width, this.canvas.height)
      this.renderElement(g)
   }

   renderElement (g: GraphicsElement): void {
      if (g instanceof Graphic) {   
         for (let gs: List<GraphicsElement> = g.gs; Cons.is(gs); gs = gs.tail) {
            this.renderElement(gs.head)
         }
      } else 
      if (g instanceof PathStroke) {
         this.pathStroke(g.points)
      } else
      if (g instanceof RectFill) {
         this.rectFill(g.points)
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

   renderWith (g: GraphicsElement, f: TransformFun): void {
      const transform: TransformFun = this.transform
      this.transforms.push(postcompose(transform, f))
      this.renderElement(g)
      this.transforms.pop()
   }

   path2D (points: List<Point>): Path2D {
      const region: Path2D = new Path2D
      if (Cons.is(points)) {
         const [x, y]: [number, number] = this.transform([points.head.x.val, points.head.y.val])
         region.moveTo(x, y)
         points = points.tail
         for (; Cons.is(points); points = points.tail) {
            const [x, y]: [number, number] = this.transform([points.head.x.val, points.head.y.val])
            region.lineTo(x, y)
         }
      } else {
         return absurd()
      }
      return region
   }

   svgPath (p̅: List<Point>): [number, number][] {
      return p̅.toArray().map(({ x, y }): [number, number] => this.transform([x.val, y.val]))
   }

   pathStroke (p̅: List<Point>): void {
      const path = document.createElementNS("http://www.w3.org/2000/svg", "polyline"),
            p̅_str: string = this.svgPath(p̅).map((xy: [number, number]) => xy.join()).join(" ")
      path.setAttributeNS(null, "points", p̅_str)
      path.setAttributeNS(null, "fill", "none")
      path.setAttributeNS(null, "stroke", "green")
      this.svg.appendChild(path)

      const region: Path2D = this.path2D(p̅)
      this.ctx.strokeStyle = "black"
      this.ctx.stroke(region)
      this.pointHighlights(p̅)
   }

   pointHighlights (points: List<Point>): void {
      for (; Cons.is(points); points = points.tail) {
         const point: Point = points.head,
               [x, y]: [number, number] = this.transform([point.x.val, point.y.val])
         if (!__nonNull(asVersioned(point.x).__α) || !__nonNull(asVersioned(point.y).__α)) {
            this.circle(x, y, 3)
         }
      }
   }

   circle (x: number, y: number, radius: number): void {
      this.ctx.beginPath()
      this.ctx.arc(x, y, radius, 0, 2 * Math.PI)
      this.ctx.strokeStyle = "#0000ff"
      this.ctx.stroke()
   }

   rectFill (rect_path: List<Point>): void {
      const region: Path2D = this.path2D(rect_path)
      this.ctx.fillStyle = "#f6831e"
      this.ctx.fill(region)

      const rect: SVGRectElement = document.createElementNS("http://www.w3.org/2000/svg", "rect"),
            p̅: [number, number][] = this.svgPath(rect_path)

      rect.setAttribute("x", p̅[0][0].toString())
      rect.setAttribute("y", (200 - p̅[2][1]).toString()) // TODO: hardcoded to height of viewport
      rect.setAttribute("width", (p̅[1][0] - p̅[0][0]).toString())
      rect.setAttribute("height", (p̅[2][1] - p̅[0][1]).toString())
      rect.setAttribute("fill", "#f6831e")
      this.svg.appendChild(rect)
   }
}
