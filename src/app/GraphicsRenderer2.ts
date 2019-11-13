import { __nonNull, absurd, as, assert } from "../util/Core"
import { Cons, List, Option, Pair, Some } from "../BaseTypes"
import { ExplValue } from "../DataValue"
import { Group, GraphicsElement, Polyline, Rect, Scale, Transform, Translate } from "../Graphics2"
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

type TransformFun = (x: number, y: number) => [number, number]

function scale (x_scale: number, y_scale: number): TransformFun {
   return (x, y): [number, number] => {
      return [x * x_scale, y * y_scale]
   }
}

function translate (x_inc: number, y_inc: number): TransformFun {
   return (x, y): [number, number] => {
      return [x + x_inc, y + y_inc]
   }
}

function transformFun (t: Transform): TransformFun {
   if (t instanceof Scale) {
      return scale(t.x.val, t.y.val)
   } else
   if (t instanceof Translate) {
      return translate(t.x.val, t.y.val)
   } else {
      return absurd()
   }
}

function postcompose (f1: TransformFun, f2: TransformFun): TransformFun {
   return (x, y): [number, number] => {
      let [x_, y_] = f2(x, y)
      return f1(x_, y_)
   }
}

export class GraphicsRenderer {
   transforms: TransformFun[] // stack of successive compositions of linear transformations
   transforms_: Transform[] // stack of original transform objects for debugging
   ancestors: SVGElement[] // stack of enclosing SVG elements

   constructor (root: SVGElement) {
      this.ancestors = [root]
      this.transforms = [(x, y) => [x, y]]
      this.transforms_ = [] // indices will be out by two, but only for debugging..
   }

   get current (): SVGElement {
      return this.ancestors[this.ancestors.length - 1]
   }

   get transform (): TransformFun {
      assert(this.transforms.length > 0)
      // query the current transform rather than returing a closure that accesses it...
      const transform: TransformFun = this.transforms[this.transforms.length - 1]
      return (x, y) => {
         const [xʹ, yʹ] = transform(x, y)
         return [Math.round(xʹ), Math.round(yʹ)]
      } 
   }

   render (tg: ExplValue<GraphicsElement>, [w, h]: [number, number]): void {
      assert(this.ancestors.length === 1)
      const root: SVGElement = this.current
      while (root.firstChild !== null) {
         root.removeChild(root.firstChild)
      }
      const width: number = parseFloat(__nonNull(root.getAttribute("width")))
      const height: number = parseFloat(__nonNull(root.getAttribute("height")))
      // TODO: use withLocalTransform here
      this.transforms.push(postcompose(this.transform, scale(width / w, height / h)))
      this.renderElement(ExplValueCursor.descendant(null, tg))
      this.transforms.pop()
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

   withLocalTransforms<T> (ts: Option<Transform>[], localRender: () => T): T {
      const ts_: Transform[] = ts.filter(t => t instanceof Some).map(t => as(as(t, Some).t, Transform))
      let result: T
      ts_.forEach(t => {
         this.transforms.push(postcompose(this.transform, transformFun(t)))
         this.transforms_.push(t)
      })
      result = localRender()
      ts_.forEach(_ => {
         this.transforms.pop()
         this.transforms_.pop()
      })
      return result
   }

   group (tg: ExplValueCursor/*<Graphic>*/): void {
      const svg: SVGSVGElement = document.createElementNS(SVG.NS, "svg")
      const g: Group = as(tg.tv.v, Group)
      const [x, y] = this.transform(g.x.val, g.y.val)
      // x and y attributes are relative to parent coordinate space, so not transformed.
      // width and height refer to size of viewport (again in parent coordinate space), although currently
      // we ignore these; we should really clip the child content.
      svg.setAttribute("x", `${x}`)
      svg.setAttribute("y", `${y}`)
      this.current.appendChild(svg)
      this.ancestors.push(svg)
      this.withLocalTransforms([g.scale, g.translate], () => { // scaling applies to translated coordinates
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
      const [x, y] = this.transform(g.x.val, g.y.val)
      const [width, height] = this.transform(g.width.val, g.height.val)
      rect.setAttribute("x", `${x}`)
      rect.setAttribute("y", `${y}`)
      rect.setAttribute("width", `${width}`)
      rect.setAttribute("height", `${height}`)
      rect.setAttribute("fill", g.fill.val)
      this.current.appendChild(rect)
   }

   polyline (tg: ExplValueCursor/*<Polyline>*/): void {
      const g: Polyline = as(tg.tv.v, Polyline)
      // each point is considered a "child", and therefore subject to my local scaling
      const ps: [number, number][] = this.withLocalTransforms([g.scale], () => {
         return g.points.toArray().map((p: Pair<Num, Num>): [number, number] => {
            return this.transform(p.fst.val, p.snd.val)
         })
      })
      // experiment with optimising pair case to line rather than polyline
      // TODO: what should we do when there is only a single point?
      let path: SVGElement
      if (ps.length === 2) {
         path = document.createElementNS(SVG.NS, "line")
         const [[x1, y1], [x2, y2]] = ps
         path.setAttribute("x1", `${x1}`)
         path.setAttribute("y1", `${y1}`)
         path.setAttribute("x2", `${x2}`)
         path.setAttribute("y2", `${y2}`)
      } else {
         path = document.createElementNS(SVG.NS, "polyline")
         path.setAttribute("points", asString(ps))
      }
      path.setAttribute("stroke", g.stroke.val)
      path.setAttribute("fill", "none")
      this.current.appendChild(path)
}
}

function asString (p̅: [number, number][]): string {
   return p̅.map(([x, y]: [number, number]) => `${x},${y}`).join(" ")
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
