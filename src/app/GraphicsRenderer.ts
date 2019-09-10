import { __nonNull, absurd, assert } from "../util/Core"
import { Annotation, ann } from "../util/Lattice"
import { asAnnotated, num, setallα } from "../Annotated"
import { Cons, List } from "../BaseTypes"
import { Expl_ } from "../DataValue"
import { Direction } from "../Eval"
import { Graphic, GraphicsElement, Polygon, Polyline, Point, Text, Translate } from "../Graphics"
import { unary_, unaryOps } from "../Primitive"
import { Num, Str } from "../Value"

export const svgNS: "http://www.w3.org/2000/svg" = "http://www.w3.org/2000/svg"
type TransformFun = (p: [number, number]) => [number, number]

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

export interface Slicer {
   fwdSlice (): void    // fwd slice and set direction to fwd
   resetForBwd (): void // set all annotations to bot
   bwdSlice (): void    // bwd slice and set direction to bwd
   direction: Direction
   coordinator: ViewCoordinator
}

export interface ViewCoordinator {
   onBwd (): void
   resetForBwd (): void
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
      assert(this.ancestors.length === 1      // In the bwd direction, a point appears "needed" (true) if either of its components is needed.
   )
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
         this.polygon(g)
      } else
      if (g instanceof Text) {
         this.text(g)
      } else
      if (g instanceof Translate) {
         this.renderWith(g.g, translate(g.x.val, g.y.val))
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
         this.slicer.coordinator.resetForBwd()
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
         // TODO: annotation on point itself is not considered yet
         this.xyHighlight(p̅.head.explChild("x", Num), p̅.head.explChild("y", Num))
      }
   }

   xyHighlight (tx: Expl_<Num>, ty: Expl_<Num>): void {
      const [x_α, y_α] = [__nonNull(tx.t.__α), __nonNull(ty.t.__α)]
      let α: Annotation = ann.meet(x_α, y_α)
      if (this.slicer.direction === Direction.Fwd) {
         α = ann.negate(α)
      }
      if (α) {
         const [xʹ, yʹ]: [number, number] = this.transform([tx.v.val, ty.v.val])
         this.circle(xʹ, yʹ, 3)
      }
   }

   circle (x: number, y: number, radius: number): void {
      const circle: SVGCircleElement = document.createElementNS(svgNS, "circle")
      circle.setAttribute("cx", x.toString())
      circle.setAttribute("cy", y.toString())
      circle.setAttribute("r", radius.toString())
      circle.setAttribute("stroke", "blue")
      circle.setAttribute("fill", "none")
      this.current.appendChild(circle)
   }

   polygon (g: Polygon): void {
      const polygon: SVGPolygonElement = document.createElementNS(svgNS, "polygon")
      polygon.setAttribute("points", this.points(g.points))
      polygon.setAttribute("stroke", g.stroke.val)
      polygon.setAttribute("fill", g.fill.val)
      polygon.addEventListener("click", (e: MouseEvent): void => {
         e.stopPropagation()
         this.slicer.coordinator.resetForBwd()
         g.points.toArray().map((p: Point): void => {
            setallα(ann.top, p)
         })
         this.slicer.bwdSlice()
      })
      this.current.appendChild(polygon)
      this.pointHighlights(g.points)
   }

   // Flip text vertically to cancel out the global vertical flip. Don't set x and y but express
   // position through a translation so that the scaling doesn't affect the position.
   text (g: Text): void {
      const [x, y]: [number, number] = this.transform([g.x.val, g.y.val]),
            text: SVGTextElement = textElement(x, y, g.str.val)
      text.addEventListener("click", (e: MouseEvent): void => {
         e.stopPropagation()
         this.slicer.coordinator.resetForBwd()
         setallα(ann.top, g)
         this.slicer.bwdSlice()
      })
      this.current.appendChild(text)
      // this.xyHighlight(g.x, g.y)
      // TODO: annotation on text element itself is not considered yet
      let α: Annotation = __nonNull(asAnnotated(g.str).__α)
      if (this.slicer.direction === Direction.Fwd) {
         α = ann.negate(α)
      }
      if (α) {
         const bbox: SVGRect = text.getBBox(),
               rect = document.createElementNS(svgNS, "rect")
             rect.setAttribute("x", bbox.x.toString())
             rect.setAttribute("y", bbox.y.toString())
             rect.setAttribute("width", bbox.width.toString())
             rect.setAttribute("height", bbox.height.toString())
             rect.setAttribute("fill", "#D3D3D3")
             rect.setAttribute("transform", `translate(${x.toString()},${y.toString()}) scale(1,-1)`)
             this.current.insertBefore(rect, text)
         text.setAttribute("fill", "blue")
      } else {
         text.setAttribute("fill", "black")
      }
   }
}

// The SVG text element for the supplied text; centralised so can be used to compute text metrics.
// Use "translate" to locate the element, so that we can apply it after scaling.
function textElement (x: number, y: number, str: string): SVGTextElement {
   const text: SVGTextElement = document.createElementNS(svgNS, "text")
   text.setAttribute("stroke", "none")
   text.setAttribute("font-size", "12")
   text.setAttribute("transform", `translate(${x.toString()},${y.toString()}) scale(1,-1)`)
   text.appendChild(document.createTextNode(str))
   return text
}

let svgMetrics: SVGSVGElement

{
   svgMetrics = document.createElementNS(svgNS, "svg")
   svgMetrics.setAttribute("width", "0")
   svgMetrics.setAttribute("height", "0")
   svgMetrics.style.visibility = "hidden"
   document.body.appendChild(svgMetrics)

   // Additional primitives that rely on offline rendering to compute text metrics. Combine these would 
   // require more general primitives that can return tuples.
   const textWidth = (str: Str): Num => {
      const text: SVGTextElement = textElement(0, 0, str.val)
      svgMetrics.appendChild(text)
      const width: number = text.getBBox().width
      text.remove()
      return num(width)
   }
   
   const textHeight = (str: Str): Num => {
      const text: SVGTextElement = textElement(0, 0, str.val)
      svgMetrics.appendChild(text)
      const height: number = text.getBBox().height
      text.remove()
      return num(height)
   }
   
   unaryOps.set(textHeight.name, unary_(textHeight))
   unaryOps.set(textWidth.name, unary_(textWidth))
}
