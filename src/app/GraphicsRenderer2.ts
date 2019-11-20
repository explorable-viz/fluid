import { last } from "../util/Array"
import { Class, __nonNull, absurd, as, assert, id, userError } from "../util/Core"
import { Cons, List, Pair } from "../BaseTypes"
import { ExplValue } from "../DataValue"
import { Circle, Group, GraphicsElement, Line, Marker, Polyline, Polymarkers, Rect, Scale, Text, Transform, Translate, Viewport } from "../Graphics2"
import { Unary, unary_, unaryOps } from "../Primitive"
import { Id, Num, Str } from "../Value"
import { num } from "../Versioned"
import { SVG } from "./Core"
import { ExplValueCursor } from "./Cursor"
import { border, circle, line, markerEnsureDefined, polyline, rect, svgElement, textElement_graphical } from "./Renderer"

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
   root: SVGSVGElement
   ancestors: SVGElement[] // stack of enclosing SVG elements
   translations: TransformFun[] // stack of (uncomposed) active translations, each relative to parent SVG
   scalings: TransformFun[] // stack of successively composed scalings, each relative to root SVG
   showInvisible: boolean = false

   // transform attribute isn't supported on SVGElement, so it contains a group element with the inversion transform.
   constructor (root: SVGSVGElement, initialAncestor: SVGElement) {
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

   renderElement (tg: ExplValueCursor/*<GraphicsElement>*/): void {
      const g: GraphicsElement = as(tg.tv.v, GraphicsElement)
      if (g instanceof Circle) {
         this.circle(tg)
      } else 
      if (g instanceof Group) {
         this.group(tg)
      } else 
      if (g instanceof Line) {
         this.line(tg)
      } else
      if (g instanceof Polyline) {
         this.polyline(tg)
      } else
      if (g instanceof Polymarkers) {
         this.polymarkers(tg)
      } else
      if (g instanceof Rect) {
         this.rect(tg)
      } else
      if (g instanceof Text) {
         this.text(tg)
      } else
      if (g instanceof Viewport) {
         this.viewport(tg)
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
   circle (tg: ExplValueCursor/*<Rect>*/): void {
      const g: Circle = as(tg.tv.v, Circle)
      const [x, y] = this.transform([g.x.val, g.y.val])
      const [x_scale, y_scale] = this.scale([1, 1])
      const r: SVGCircleElement = circle(x, y, g.radius.val * x_scale * y_scale, "none", g.fill.val, this.circle)
      this.current.appendChild(r)
   }

   group (tg: ExplValueCursor/*<Group>*/): void {
      for (let tg̅: ExplValueCursor/*<List<GraphicsElement>>*/ = tg.to(Group, "gs"); 
           Cons.is(as(tg̅.tv.v, List)); tg̅ = tg̅.to(Cons, "tail")) {
         this.renderElement(tg̅.to(Cons, "head"))
      }
   }

   // For line/polyline, each point is considered a "child", and therefore subject to my local scaling.
   line (tg: ExplValueCursor/*<Polyline>*/): void {
      const g: Line = as(tg.tv.v, Line)
      const [[x1, y1], [x2, y2]] = [
         this.transform([g.p1.fst.val, g.p1.snd.val]), 
         this.transform([g.p2.fst.val, g.p2.snd.val])
      ]
      this.current.appendChild(line(x1, y1, x2, y2, g.stroke.val, g.strokeWidth.val))
   }

   polyline (tg: ExplValueCursor/*<Polyline>*/): void {
      const g: Polyline = as(tg.tv.v, Polyline)
      const ps: [number, number][] = g.points.toArray().map((p: Pair<Num, Num>): [number, number] => {
         return this.transform([p.fst.val, p.snd.val])
      })
      this.current.appendChild(polyline(ps, g.stroke.val, g.strokeWidth.val))
   }

   // Polymarkers have coordinates relative to the points, in the *parent* scaling.
   polymarkers (tg: ExplValueCursor/*<Polymarkers>*/): void {
      for (let tg̅: ExplValueCursor/*<List<GraphicsElement>>*/ = tg.to(Polymarkers, "markers"),
               tps: ExplValueCursor/*<List<Pair<Num, Num>>*/ = tg.to(Polymarkers, "points"); 
           Cons.is(as(tg̅.tv.v, List)) || Cons.is(as(tps.tv.v, List)); 
           tg̅ = tg̅.to(Cons, "tail"), tps = tps.to(Cons, "tail")) {
         if (!Cons.is(as(tg̅.tv.v, List)) || !Cons.is(as(tps.tv.v, List))) {
            userError(`${Polymarkers.name}: more markers than points.`)
         } else {
            const p: Pair<Num, Num> = as(tps.to(Cons, "head").tv.v, Pair)
            const [x, y] = this.transform([p.fst.val, p.snd.val])
            const svg: SVGSVGElement = svgElement(x, y, 10, 10, false, this.polymarkers)
            this.current.appendChild(svg)
            this.ancestors.push(svg)
            this.withLocalFrame(
               invertScale(this.scale),
               id, 
               () => {
                  this.renderElement(tg̅.to(Cons, "head"))
               }
            )
            this.ancestors.pop()
         }
      }
   }

   rect (tg: ExplValueCursor/*<Rect>*/): void {
      const g: Rect = as(tg.tv.v, Rect)
      const [x, y] = this.transform([g.x.val, g.y.val])
      const [width, height] = this.scale([g.width.val, g.height.val])
      assert(width >= 0 && height >= 0)
      const r: SVGRectElement = rect(x, y, width, height, "none", g.fill.val, this.rect)
      this.current.appendChild(r)
   }

   text (tg: ExplValueCursor/*<Text>*/): void {
      const g: Text = as(tg.tv.v, Text),
            [x, y]: [number, number] = this.transform([g.x.val, g.y.val]),
            text: SVGTextElement = textElement_graphical(x, y, fontSize, g.str.val)
      this.current.appendChild(text)
      text.setAttribute("fill", "black")
      text.setAttribute("text-anchor", `${g.anchor.val}`)
      text.setAttribute("alignment-baseline", `${g.baseline.val}`)
   }

   // let margin = fun m Viewport(x, y, w, h, fill, Scale(x_scale, y_scale), Translate(dx, dy), gs) →
   //   let x_scale' = x_scale * max2 (w - 2 * m, 0) / w;
   //   let y_scale' = y_scale * max2 (h - 2 * m, 0) / h;
   //   let translate = Translate(dx + m / x_scale', dy + m / y_scale')
   //   in Viewport(x, y, w, h, fill, Scale(x_scale', y_scale'), translate, gs);
   
   viewport (tg: ExplValueCursor/*<Viewport>*/): void {
      const g: Viewport = as(tg.tv.v, Viewport)
      // dimensions are relative to parent coordinate space, so not transformed by g's scaling
      const [x, y] = this.transform([g.x.val, g.y.val])
      const [width, height] = this.scale([g.width.val, g.height.val])
      assert(width >= 0 && height >= 0)
      const outerSvg: SVGSVGElement = svgElement(x, y, width, height, false, this.viewport)
      if (g.fill.val !== "none") {
         this.current.appendChild(rect(x, y, width, height, "none", g.fill.val, this.viewport))
      }
      this.current.appendChild(outerSvg)
      if (this.showInvisible) {
         this.current.appendChild(border(x, y, width, height, "gray", true))
      }
      this.ancestors.push(outerSvg)
      const m: number = g.margin.val
      // TODO: check for zero w or h
      const [w, h]: [number, number] = [Math.max(width - m * 2), height - m * 2]
      const innerViewport: SVGSVGElement = svgElement(m, m, w, h, false, this.viewport)
      this.current.appendChild(innerViewport)
      this.ancestors.push(innerViewport)
      this.withLocalFrame(
         transformFun(g.scale), 
         transformFun(g.translate), 
         () => {
            for (let tg̅: ExplValueCursor/*<List<GraphicsElement>>*/ = tg.to(Viewport, "gs"); 
                 Cons.is(as(tg̅.tv.v, List)); tg̅ = tg̅.to(Cons, "tail")) {
               this.renderElement(tg̅.to(Cons, "head"))
            }
         }
      )
      this.ancestors.pop()
      this.ancestors.pop()
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
