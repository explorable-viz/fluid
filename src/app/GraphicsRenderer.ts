import { __nonNull, absurd, as, assert } from "../util/Core"
import { Annotation, ann } from "../util/Lattice"
import { setα } from "../Annotated"
import { Cons, List, Some } from "../BaseTypes"
import { ExplValue } from "../DataValue"
import { Direction } from "../Eval"
import { Graphic, GraphicsElement, Polygon, Polyline, Point, Text, Translate } from "../Graphics"
import { Unary, unary_, unaryOps } from "../Primitive"
import { Id, Num, Str } from "../Value"
import { num } from "../Versioned"
import { SVG } from "./Core"
import { ExplValueCursor } from "./Cursor"

export const svg: SVG = new SVG()
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
   ancestors: SVGElement[] // stack of enclosing SVG elements
   slicer: Slicer

   constructor (root: SVGElement, slicer: Slicer) {
      this.ancestors = [root]
      this.slicer = slicer
      this.transforms = [x => x]
   }

   get current (): SVGElement {
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

   render (tg: ExplValue<GraphicsElement>): void {
      assert(this.ancestors.length === 1)
      while (this.current.firstChild !== null) {
         this.current.removeChild(this.current.firstChild)
      }
      this.renderElement(ExplValueCursor.descendant(null, tg))
   }

   renderElement (tg: ExplValueCursor/*<GraphicsElement>*/): void {
      const g: GraphicsElement = as(tg.tv.v, GraphicsElement)
      if (g instanceof Graphic) {
         this.group(tg)
      } else 
      if (g instanceof Polyline) {
         this.polyline(tg)
      } else
      if (g instanceof Polygon) {
         this.polygon(tg)
      } else
      if (g instanceof Text) {
         this.text(tg)
      } else
      if (g instanceof Translate) {
         this.translate(tg)
      }
      else {
         return absurd()
      }
   }

   group (tg: ExplValueCursor/*<Graphic>*/): void {
      const group: SVGGElement = document.createElementNS(SVG.NS, "g")
      // See https://www.smashingmagazine.com/2018/05/svg-interaction-pointer-events-property/.
      group.setAttribute("pointer-events", "bounding-box")
      this.current.appendChild(group)
      this.ancestors.push(group)
      // ignoring annotations on cons cells
      for (let tg̅: ExplValueCursor/*<List<GraphicsElement>>*/ = tg.to(Graphic, "gs"); 
           Cons.is(as(tg̅.tv.v, List)); tg̅ = tg̅.to(Cons, "tail")) {
         this.renderElement(tg̅.to(Cons, "head"))
      }
      this.ancestors.pop()
   }

   translate (tg: ExplValueCursor/*<Translate>*/): void {
      const g: Translate = as(tg.tv.v, Translate),
            tgʹ: ExplValueCursor/*<GraphicsElement>*/ = tg.to(Translate, "g"), 
            f: TransformFun = translate(g.x.val, g.y.val)
      const transform: TransformFun = this.transform
      this.transforms.push(postcompose(transform, f))
      this.renderElement(tgʹ)
      this.transforms.pop()
   }

   transformedPath (p̅: List<Point>): [number, number][] {
      return p̅.toArray().map(({ x, y }): [number, number] => this.transform([x.val, y.val]))
   }

   transformedScaledPath (p̅: List<Point>): [number, number][] {
      return p̅.toArray().map(({ x, y }): [number, number] => this.transform([x.val * 40, y.val * 0.15]))
   }

   asString (p̅: [number, number][]): string {
      return p̅.map(([x, y]: [number, number]) => `${x},${y}`).join(" ")
   }

   polyline (tg: ExplValueCursor/*<Polyline>*/): void {
      const path: SVGPolylineElement = document.createElementNS(SVG.NS, "polyline")
      const g: Polyline = as(tg.tv.v, Polyline)
      const ps: [number, number][] = this.transformedScaledPath(g.points)
      if (Some.is(g.marker)) {
         ps.map(([x, y]) => this.plotPoint(2.5, x, y)) // hardcoded radius for now
      }
      path.setAttribute("points", this.asString(ps))
      path.setAttribute("stroke", "black")
      path.addEventListener("click", (e: MouseEvent): void => {
         e.stopPropagation()
         assert(false, "Not implemented yet")
      })
      this.current.appendChild(path)
      this.pointHighlights(tg.to(Polyline, "points"))
   }

   line (x1: number, y1: number, x2: number, y2: number): void {
      const line: SVGLineElement = document.createElementNS(SVG.NS, "line")
      line.setAttribute("x1", `${x1}`)
      line.setAttribute("y1", `${y1}`)
      line.setAttribute("x2", `${x2}`)
      line.setAttribute("y2", `${y2}`)
      line.setAttribute("stroke", "black")
      this.current.appendChild(line)
   }

   plotPoint (radius: number, x: number, y: number): void {
      this.line(x - radius, y - radius, x + radius, y + radius)
      this.line(x - radius, y + radius, x + radius, y - radius)
   }

   pointHighlights (tp̅: ExplValueCursor/*<List<Point>>*/): void {
      for (; Cons.is(as(tp̅.tv.v, List)); tp̅ = tp̅.to(Cons, "tail")) {
         // TODO: annotation on point itself is not considered yet
         const p: ExplValueCursor/*<Point>*/ = tp̅.to(Cons, "head")
         this.xyHighlight(p.to(Point, "x"), p.to(Point, "y"))
      }
   }

   xyHighlight (tx: ExplValueCursor/*<Num>*/, ty: ExplValueCursor/*<Num>*/): void {
      const [x_α, y_α] = [__nonNull(tx.tv.t).__α, __nonNull(ty.tv.t).__α]
      let α: Annotation = ann.meet(x_α, y_α)
      if (this.slicer.direction === Direction.Fwd) {
         α = ann.negate(α)
      }
      if (α) {
         const [xʹ, yʹ]: [number, number] = this.transform([as(tx.tv.v, Num).val, as(ty.tv.v, Num).val])
         this.circle(xʹ, yʹ, 3)
      }
   }

   circle (x: number, y: number, radius: number): void {
      const circle: SVGCircleElement = document.createElementNS(SVG.NS, "circle")
      circle.setAttribute("cx", x.toString())
      circle.setAttribute("cy", y.toString())
      circle.setAttribute("r", radius.toString())
      circle.setAttribute("stroke", "blue")
      circle.setAttribute("fill", "none")
      this.current.appendChild(circle)
   }

   polygon (tg: ExplValueCursor/*<Polygon>*/): void {
      const polygon: SVGPolygonElement = document.createElementNS(SVG.NS, "polygon"),
            g: Polygon = as(tg.tv.v, Polygon)
      polygon.setAttribute("points", this.asString(this.transformedScaledPath(g.points)))
      polygon.setAttribute("stroke", g.stroke.val)
      polygon.setAttribute("fill", g.fill.val)
      polygon.addEventListener("click", (e: MouseEvent): void => {
         e.stopPropagation()
         this.slicer.coordinator.resetForBwd()
         // set annotations only on _points_, not list containing them or polygon itself
         for (let tp̅: ExplValueCursor/*<List<Point>>*/ = tg.to(Polygon, "points"); 
              Cons.is(as(tp̅.tv.v, List)); tp̅ = tp̅.to(Cons, "tail")) {
            const tp: ExplValueCursor/*<Point>*/ = tp̅.to(Cons, "head")
            setα(ann.top, tp.tv.t)
            setα(ann.top, tp.to(Point, "x").tv.t)
            setα(ann.top, tp.to(Point, "y").tv.t)
         }
         this.slicer.bwdSlice()
      })
      this.current.appendChild(polygon)
      this.pointHighlights(tg.to(Polygon, "points"))
   }

   // Flip text vertically to cancel out the global vertical flip. Don't set x and y but express
   // position through a translation so that the scaling doesn't affect the position.
   text (tg: ExplValueCursor/*<Text>*/): void {
      const g: Text = as(tg.tv.v, Text),
            [x, y]: [number, number] = this.transform([g.x.val, g.y.val]),
            text: SVGTextElement = textElement(x, y, fontSize, g.str.val)
      text.addEventListener("click", (e: MouseEvent): void => {
         e.stopPropagation()
         this.slicer.coordinator.resetForBwd()
         setα(ann.top, tg.tv.t)
         setα(ann.top, tg.to(Text, "str").tv.t)
         this.slicer.bwdSlice()
      })
      this.current.appendChild(text)
      // this.xyHighlight(g.x, g.y)
      // TODO: annotation on text element itself is not considered yet
      let α: Annotation = __nonNull(tg.to(Text, "str").tv.t.__α)
      if (this.slicer.direction === Direction.Fwd) {
         α = ann.negate(α)
      }
      if (α) {
         const bbox: SVGRect = text.getBBox(),
               rect = document.createElementNS(SVG.NS, "rect")
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
