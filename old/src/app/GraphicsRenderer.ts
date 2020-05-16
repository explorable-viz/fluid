import { last } from "../util/Array"
import { Class, __log, __nonNull, absurd, as, assert, id, userError } from "../util/Core"
import { Cons, List } from "../BaseTypes"
import { ExplValue } from "../DataValue"
import { Circle, Group, GraphicsElement, Line, Marker, Polyline, Polymarkers, Point, Rect, Scale, Text, Transform, Translate, Viewport } from "../Graphics"
import { Unary, unary_, unaryOps } from "../Primitive"
import { Id, Num, Str } from "../Value"
import { num } from "../Versioned"
import { SVG } from "./Core"
import { ExplValueCursor } from "./Cursor"
import { PointInteractor, RectInteractor } from "./Interactor"
import { Pane } from "./Pane"
import { border, circle, group, lineRounded, markerEnsureDefined, polyline, rect, svgElement, textElement_graphical } from "./Renderer"

const fontSize: number = 12
export const svg: SVG = new SVG()

type TransformFun = ([x, y]: [number, number]) => [number, number]

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

function invertScale (scale: TransformFun): TransformFun {
   return ([x, y]): [number, number] => {
      const [x_scale, y_scale]: [number, number] = scale([1, 1])
      return [x / x_scale, y / y_scale]
   }
}

function transformFun (t: Transform): TransformFun {
   if (t instanceof Scale) {
      assert(t.x.val >= 0 && t.y.val >= 0)
      return scale(t.x.val, t.y.val)
   } else
   if (t instanceof Translate) {
      assert(isFinite(t.x.val) && isFinite(t.y.val))
      return translate(t.x.val, t.y.val)
   } else {
      return absurd()
   }
}

function postcompose (f1: TransformFun, f2: TransformFun): TransformFun {
   return ([x, y]): [number, number] => {
      return f1(f2([x, y]))
   }
}

export class GraphicsRenderer {
   editor: Pane.Pane
   root: SVGSVGElement
   ancestors: SVGElement[] // stack of enclosing SVG elements
   translations: TransformFun[] // stack of (uncomposed) active translations, each relative to parent SVG
   scalings: TransformFun[] // stack of successively composed scalings, each relative to root SVG
   showInvisible: boolean = false

   // transform attribute isn't supported on SVGElement, so it contains a group element with the inversion transform.
   constructor (editor: Pane.Pane, root: SVGSVGElement, initialAncestor: SVGElement) {
      this.editor = editor
      this.root = root
      this.ancestors = [initialAncestor]
      this.translations = [id]
      this.scalings = [id]
   }

   get current (): SVGElement {
      return this.ancestors[this.ancestors.length - 1]
   }

   // scaling applies to translated coordinates
   get transform (): TransformFun {
      return postcompose(this.scale, last(this.translations))
   }

   get scale (): TransformFun {
      return last(this.scalings)
   }

   render (tg: ExplValue<GraphicsElement>, [w, h]: [number, number]): void {
      assert(this.ancestors.length === 1)
      const root: SVGElement = this.current
      while (root.firstChild !== null) {
         root.removeChild(root.firstChild)
      }
      const width: number = parseFloat(__nonNull(root.getAttribute("width")))
      const height: number = parseFloat(__nonNull(root.getAttribute("height")))
      this.withLocalFrame(
         scale(width / w, height / h),
         id,
         () => {
            this.renderElement(ExplValueCursor.descendant(null, tg))
         }
      )
   }

   renderElement (tg: ExplValueCursor/*<GraphicsElement>*/): SVGElement {
      const g: GraphicsElement = as(tg.tv.v, GraphicsElement)
      if (g instanceof Circle) {
         return this.circle(tg)
      } else 
      if (g instanceof Group) {
         return this.group(tg)
      } else 
      if (g instanceof Line) {
         return this.line(tg)
      } else
      if (g instanceof Polyline) {
         return this.polyline(tg)
      } else
      if (g instanceof Polymarkers) {
         return this.polymarkers(tg)
      } else
      if (g instanceof Rect) {
         return this.rect(tg)
      } else
      if (g instanceof Text) {
         return this.text(tg)
      } else
      if (g instanceof Viewport) {
         return this.viewport(tg)
      } else {
         return absurd()
      }
   }

   // Scalings accumulate as we go down. Translations don't, because we use nested SVGs.
   withLocalFrame<T> (scale: TransformFun, translate: TransformFun, localRender: () => T): T {
      let result: T
      this.scalings.push(postcompose(last(this.scalings), scale))
      this.translations.push(translate)
      result = localRender()
      this.translations.pop()
      this.scalings.pop()
      return result
   }

   // Scale circle by product of x, y scaling factors to maintain ratio of area to fixed rectangle as an invariant.
   circle (tg: ExplValueCursor/*<Rect>*/): SVGCircleElement {
      const g: Circle = as(tg.tv.v, Circle)
      const [x, y] = this.transform([g.x.val, g.y.val])
      const [x_scale, y_scale] = this.scale([1, 1])
      const r: SVGCircleElement = circle(x, y, g.radius.val * x_scale * y_scale, "none", g.fill.val, this.circle)
      this.current.appendChild(r)
      return r
   }

   group (tg: ExplValueCursor/*<Group>*/): SVGGElement {
      const g: SVGGElement = group()
      this.current.appendChild(g)
      this.ancestors.push(g)
      for (let tg̅: ExplValueCursor/*<List<GraphicsElement>>*/ = tg.to(Group, "gs"); 
           Cons.is(as(tg̅.tv.v, List)); tg̅ = tg̅.to(Cons, "tail")) {
         this.renderElement(tg̅.to(Cons, "head"))
      }
      this.ancestors.pop()
      return g
   }

   // For line/polyline, each point is considered a "child", and therefore subject to my local scaling.
   line (tg: ExplValueCursor/*<Polyline>*/): SVGLineElement {
      const g: Line = as(tg.tv.v, Line)
      const [[x1, y1], [x2, y2]] = [
         this.transform([g.p1.x.val, g.p1.y.val]), 
         this.transform([g.p2.x.val, g.p2.y.val])
      ]
      const l: SVGLineElement = lineRounded(x1, y1, x2, y2, g.stroke.val, g.strokeWidth.val)
      this.current.appendChild(l)
      return l
   }

   polyline (tg: ExplValueCursor/*<Polyline>*/): SVGPolylineElement {
      const g: Polyline = as(tg.tv.v, Polyline)
      const ps: [number, number][] = g.points.toArray().map((p: Point): [number, number] => {
         return this.transform([p.x.val, p.y.val])
      })
      const l: SVGPolylineElement = polyline(ps, g.stroke.val, g.strokeWidth.val)
      this.current.appendChild(l)
      return l
   }

   // Polymarkers have coordinates relative to the points, in the *parent* scaling.
   polymarkers (tg: ExplValueCursor/*<Polymarkers>*/): SVGGElement {
      const g: SVGGElement = group()
      this.current.appendChild(g)
      this.ancestors.push(g)
      const invScale: TransformFun = invertScale(this.scale)
      for (let tg̅: ExplValueCursor/*<List<GraphicsElement>>*/ = tg.to(Polymarkers, "markers"),
               tps: ExplValueCursor/*<List<Point>*/ = tg.to(Polymarkers, "points"); 
           Cons.is(as(tg̅.tv.v, List)) || Cons.is(as(tps.tv.v, List)); 
           tg̅ = tg̅.to(Cons, "tail"), tps = tps.to(Cons, "tail")) {
         if (!Cons.is(as(tg̅.tv.v, List)) || !Cons.is(as(tps.tv.v, List))) {
            userError(`${Polymarkers.name}: more markers than points.`)
         } else {
            const tp: ExplValueCursor/*<Point>*/ = tps.to(Cons, "head")
            const p: Point = as(tp.tv.v, Point)
            const [x, y] = this.transform([p.x.val, p.y.val])
            const markerViewport: SVGSVGElement = svgElement(true, x, y, 10, 10, false, this.polymarkers)
            this.current.appendChild(markerViewport)
            this.ancestors.push(markerViewport)
            this.withLocalFrame(
               invScale,
               id, 
               () => {
                  const marker: SVGElement = this.renderElement(tg̅.to(Cons, "head"))
                  if (marker instanceof SVGCircleElement) {
                     new PointInteractor(this.editor, tp, marker)
                  }
               }
            )
            this.ancestors.pop()
         }
      }
      this.ancestors.pop()
      return g
   }

   rect (tg: ExplValueCursor/*<Rect>*/): SVGRectElement {
      const g: Rect = as(tg.tv.v, Rect)
      const [x, y] = this.transform([g.x.val, g.y.val])
      const [width, height] = this.scale([g.width.val, g.height.val])
      assert(width >= 0 && height >= 0)
      const r: SVGRectElement = rect(x, y, width, height, "none", g.fill.val, this.rect)
      new RectInteractor(this.editor, tg, r)
      this.current.appendChild(r)
      return r
   }

   text (tg: ExplValueCursor/*<Text>*/): SVGTextElement {
      const g: Text = as(tg.tv.v, Text),
            [x, y]: [number, number] = this.transform([g.x.val, g.y.val]),
            t: SVGTextElement = textElement_graphical(x, y, fontSize, g.str.val)
      this.current.appendChild(t)
      t.setAttribute("fill", "black")
      t.setAttribute("text-anchor", `${g.anchor.val}`)
      t.setAttribute("alignment-baseline", `${g.baseline.val}`)
      return t
   }

   viewport (tg: ExplValueCursor/*<Viewport>*/): SVGSVGElement {
      const g: Viewport = as(tg.tv.v, Viewport)
      // dimensions are relative to parent coordinate space, so not transformed by g's scaling
      const [x, y] = this.transform([g.x.val, g.y.val])
      const [width, height] = this.scale([g.width.val, g.height.val])
      assert(width >= 0 && height >= 0)
      const outerSvg: SVGSVGElement = svgElement(false, x, y, width, height, false, this.viewport)
      if (g.fill.val !== "none") {
         outerSvg.appendChild(rect(0, 0, width, height, "none", g.fill.val, this.viewport))
      }
      this.current.appendChild(outerSvg)
      if (this.showInvisible) {
         this.current.appendChild(border(x, y, width, height, "gray", true))
      }
      this.ancestors.push(outerSvg)
      const margin: number = g.margin.val
      const [widthʹ, heightʹ]: [number, number] = [Math.max(width - margin * 2), height - margin * 2]
      const innerScale: TransformFun = ([x, y]: [number, number]) => {
         return [x * widthʹ / width, y * heightʹ / height]
      }
      const innerViewport: SVGSVGElement = svgElement(true, margin, margin, widthʹ, heightʹ, false, this.viewport)
      this.current.appendChild(innerViewport)
      this.ancestors.push(innerViewport)
      this.withLocalFrame(
         postcompose(innerScale, transformFun(g.scale)),
         transformFun(g.translate),
         () => {
            this.renderElement(tg.to(Viewport, "g"))
         }
      )
      this.ancestors.pop()
      this.ancestors.pop()
      return outerSvg
   }

   setMarkerMid (el: SVGElement, C: Class<Marker>, colour: string): void {
      const markerId: string = markerEnsureDefined(this.root, C, colour) // revisit cast
      el.setAttribute("marker-mid", `url(#${markerId})`)
   }
}

{
   // Additional primitives that rely on offline rendering to compute text metrics. Combining these would 
   // require more general primitives that can return tuples.
   const textWidth: Unary<Str, Num> = (str: Str): (k: Id) => Num => {
      return num(svg.textWidth(textElement_graphical(0, 0, fontSize, str.val)))
   }
   
   const textHeight: Unary<Str, Num> = (str: Str): (k: Id) => Num => {
      return num(svg.textHeight(textElement_graphical(0, 0, fontSize, str.val)))
   }
   
   unaryOps.set(textWidth.name, unary_(textWidth))
   unaryOps.set(textHeight.name, unary_(textHeight))
}
