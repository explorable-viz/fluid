import { Class, __nonNull, absurd, as, assert, className } from "../util/Core"
import { Change, New, Reclassify } from "../Delta"
import { strings } from "../Expr"
import { Arrowhead, Circle, Marker } from "../Graphics2"
import { Value, isPrim } from "../Value"
import { versioned } from "../Versioned"
import { SVG } from "./Core"
import "./styles.css"

// Maybe there are some built-in types for this, but don't care yet
type Point = { x: number, y: number }
type Dims = Point & { width: number, height: number }

export const svg: SVG = new SVG()
const fontSize: number = 18
const classes: string = "code"
// bizarrely, if I do this later, font metrics are borked:
const lineHeight = svg.textHeight(textElement(fontSize, classes, "m")) // representative character 
// ASCII spaces seem to be trimmed; only Unicode space that seems to render monospaced is this: 
const space_char: string = "\u00a0"

// Populate explicity, rather than using a memoised function.
type Dimensions = { width: number, height: number }
export const dimensions: Map<SVGElement, Dimensions> = new Map()

export function arrow (ẟ_style: DeltaStyle): SVGElement {
   return keyword("arrow", ẟ_style)
}

export function border (x: number, y: number, width: number, height: number, stroke: string, dashed: boolean): SVGRectElement {
   const b: SVGRectElement = rect(x, y, width, height, stroke, "none", border)
   b.setAttribute("stroke-width", "0.5")
   if (dashed) {
      b.setAttribute("stroke-dasharray", "1,1")
   }
   return b
}

export function addBorder_changed (g: SVGSVGElement): SVGSVGElement {
   const { width, height }: Dimensions = __nonNull(dimensions.get(g))
   const b: SVGRectElement = border(g.x.baseVal.value, g.y.baseVal.value, width, height, "blue", true)
   g.appendChild(b)
   return g
}

export function addBorder_focus (g: SVGSVGElement): SVGSVGElement {
   const { width, height }: Dimensions = __nonNull(dimensions.get(g))
   const b: SVGRectElement = border(g.x.baseVal.value, g.y.baseVal.value, width, height, "gray", false)
   g.appendChild(b)
   return g
}

export function bracket (gs: SVGElement[], ẟ_style: DeltaStyle): SVGSVGElement {
   return horiz(keyword("bracketL", ẟ_style), ...gs, keyword("bracketR", ẟ_style))
}

export function centreDot (ẟ_style: DeltaStyle): SVGElement {
   return text("•", ẟ_style)
}

export function comma (ẟ_style: DeltaStyle): SVGElement {
   return keyword("comma", ẟ_style)
}

// Whether the centre of r1 is to the left of the centre of r2.
function leftOf (r1: Dims, r2: Dims): boolean {
   return r1.x + r1.width / 2 <= r2.x + r2.width
}

// TODO: remember what this is for.
function blah (x: number, length: number, proportion: number):  number {
   return x + proportion * length
}

// Offset might be better computed as a function of distance between p1 and p2.
function curvedLine (p1: Point, p2: Point, offset: number): string {
   const mp: Point = { x: (p2.x + p1.x) * 0.5, y: (p2.y + p1.y) * 0.5 }
   // angle of perpendicular to line
   const theta = Math.atan2(p2.y - p1.y, p2.x - p1.x) - Math.PI / 2
   const control: Point = { x: mp.x + offset * Math.cos(theta), y: mp.y + offset * Math.sin(theta) }
   return `M ${p1.x} ${p1.y} Q ${control.x} ${control.y} ${p2.x} ${p2.y}`
}

// Factor all element creation through this so we can tag with extra metadata.
function createElement<K extends keyof SVGElementTagNameMap>(name: K, createdBy: Function): SVGElementTagNameMap[K] {
   const e: SVGElementTagNameMap[K] = document.createElementNS(SVG.NS, name)
   e.setAttribute("data-created-by", createdBy.name)
   return e
}

export function connector (g1: SVGSVGElement, g2: SVGSVGElement): SVGElement {
   const g1_: Dims = dims(g1)
   const g2_: Dims = dims(g2)
   const [fromBottom, fromTop]: [number, number] = [0.1, 0.9]
   const connector_: SVGPathElement = createElement("path", connector)
   const curveOffset: number = 5 // somewhat arbitrary
   if (leftOf(g1_, g2_)) {
      connector_.setAttribute("d", 
         curvedLine(
            { x: g1_.x + g1_.width, y: blah(g1_.y, g1_.height, fromBottom) },
            { x: g2_.x, y: blah(g2_.y, g2_.height, fromBottom) },
            curveOffset
         )
      )
   } else {
      connector_.setAttribute("d", 
         curvedLine(
            { x: g1_.x, y: blah(g1_.y, g1_.height, fromTop) },
            { x: g2_.x + g2_.width, y: blah(g2_.y, g2_.height, fromTop) },
            curveOffset
         )
      )
   }
   connector_.setAttribute("fill", "none")
   connector_.setAttribute("stroke", "blue") // hardcoded
   connector_.setAttribute("stroke-width", "1")
   connector_.setAttribute("stroke-dasharray", "1,1")
   connector_.setAttribute("marker-end", "url(#Arrowhead-blue)") // extract to helper function
   return connector_
}

// Couldn't get getScreenCTM or getBoundingClientRect to work properly (perhaps because of nested SVGs?) so just use this to compute 
// coordinates of g relative to root SVG.
function coordinates (g: SVGSVGElement): { x: number, y: number } {
   if (g instanceof SVGSVGElement) {
      const { x, y } = g.parentElement instanceof SVGSVGElement ? coordinates(g.parentElement): { x: 0, y: 0}
      return { x: x + g.x.baseVal.value, y: y + g.y.baseVal.value }
   } else {
      return { x: 0, y: 0 }
   }
} 

export function delimit (delimiter: () => SVGElement, ...gs: SVGElement[]): SVGElement[] {
   const gsʹ: SVGElement[] = []
   gs.forEach((g: SVGElement, n: number): void => {
      gsʹ.push(g)
      if (n < gs.length - 1) {
         gsʹ.push(delimiter())
      }
   })
   return gsʹ
}

function dims (g: SVGSVGElement): Dims {
   const { width, height }: Dimensions = __nonNull(dimensions.get(g))
   const { x, y } = coordinates(g)
   return { x, y, width, height }
}

export function edge_left (g: SVGSVGElement): SVGSVGElement {
   const { height }: Dimensions = dimensions.get(g)!
   const edge: SVGLineElement = line(
      g.x.baseVal.value, 
      g.y.baseVal.value, 
      g.x.baseVal.value, 
      g.y.baseVal.value + height, 
      "gray",
      1
   )
   edge.setAttribute("stroke-width", "4")
//   edge.setAttribute("stroke-dasharray", "2,2")
   g.appendChild(edge)
   return g
}

export function edge_bottom (g: SVGSVGElement): SVGSVGElement {
   const { width, height }: Dimensions = dimensions.get(g)!
   const edge: SVGLineElement = line(
      g.x.baseVal.value, 
      g.y.baseVal.value + height, 
      g.y.baseVal.value + width, 
      g.y.baseVal.value + height, 
      "gray",
      1
   )
   edge.setAttribute("stroke-width", "2")
//   edge.setAttribute("stroke-dasharray", "2,2")
   g.appendChild(edge)
   return g
}

export function ellipsis (ẟ_style: DeltaStyle): SVGElement {
   return text("…", ẟ_style)
}

export function horiz (...gs: SVGElement[]): SVGSVGElement {
   const g: SVGSVGElement = createElement("svg", horiz)
   let width_sum: number = 0,
       height_max: number = 0
   gs.forEach((gʹ: SVGElement): void => {
      gʹ.setAttribute("x", `${width_sum}`)
      gʹ.setAttribute("y", `0`)
      const { width, height }: Dimensions = dimensions.get(gʹ)!
      width_sum += width
      height_max = Math.max(height_max, height)
      g.appendChild(gʹ)
   })
   dimensions.set(g, { width: width_sum, height: height_max })
   return g
}

export function horizSpace (...gs: SVGElement[]): SVGSVGElement {
   return horiz(...delimit(space, ...gs))
}

export function keyword (str: keyof typeof strings, ẟ_style: DeltaStyle): SVGElement {
   return text(strings[str], ẟ_style)
}

export function line (x1: number, y1: number, x2: number, y2: number, stroke: string, strokeWidth: number): SVGLineElement {
   const l: SVGLineElement = createElement("line", line)
   l.setAttribute("x1", `${round(x1)}`)
   l.setAttribute("y1", `${round(y1)}`)
   l.setAttribute("x2", `${round(x2)}`)
   l.setAttribute("y2", `${round(y2)}`)
   l.setAttribute("stroke", stroke)
   l.setAttribute("stroke-width", `${round(strokeWidth)}`)
   l.setAttribute("stroke-linecap", "round")
   return l
}

export function marker (C: Class<Marker>, colour: string): SVGMarkerElement {
   const m: SVGMarkerElement = createElement("marker", marker)
   m.setAttribute("id", markerId(C, colour))
   m.setAttribute("orient", "auto")
   m.setAttribute("fill", colour)
   m.setAttribute("stroke", colour)
   return m
}

function markerId (C: Class<Marker>, colour: string): string {
   return `${C.name}-${colour}`
}

export type MarkerFactory = (colour: string) => SVGMarkerElement

let markerFactory: Map<string, MarkerFactory>

{
   markerFactory = new Map()
   markerFactory.set(Arrowhead.name, marker_arrowhead)
   markerFactory.set(Circle.name, marker_circle)
}

// Assume root has a unique defs element called "defs". Return composite marker id.
export function markerEnsureDefined (root: SVGSVGElement, C: Class<Marker>, colour: string): string {
   const id: string = markerId(C, colour)
   let marker: Element | null = root.getElementById(id)
   if (marker === null) {
      marker = __nonNull(markerFactory.get(C.name))(colour)
      const defs: SVGDefsElement = as(root.getElementById("defs"), SVGDefsElement)
      defs.appendChild(marker)
      assert(root.getElementById(id) === marker)
   } else {
      assert(marker instanceof SVGMarkerElement)
   }
   return id
}

export function marker_arrowhead (colour: string): SVGMarkerElement {
   const m: SVGMarkerElement = marker(Arrowhead, colour)
   const length: number = 6,
         width: number = 4
   m.setAttribute("refX", `${length}`)
   m.setAttribute("refY", `${width / 2}`)
   m.setAttribute("markerWidth", "16")
   m.setAttribute("markerHeight", "16")
   const path: SVGPathElement = createElement("path", marker_arrowhead)
   m.appendChild(path)
   path.setAttribute("d", `M ${length} ${width / 2} L 0 ${width} L 0 0 Z`)
   return m
}

function marker_circle (colour: string): SVGMarkerElement {
   const m: SVGMarkerElement = marker(Circle, colour)
   const radius: number = 1
   m.setAttribute("refX", `${radius}`)
   m.setAttribute("refY", `${radius}`)
   m.setAttribute("markerWidth", `${radius * 2}`)
   m.setAttribute("markerHeight", `${radius * 2}`)
   m.setAttribute("overflow", "visible") // for debugging
   const circle: SVGCircleElement = createElement("circle", marker_circle)
   m.appendChild(circle)
   circle.setAttribute("cx", `${radius}`)
   circle.setAttribute("cy", `${radius}`)
   circle.setAttribute("r", `${radius}`)
   return m
}

export function parenthesise (g: SVGElement, ẟ_style: DeltaStyle): SVGSVGElement {
   return horiz(keyword("parenL", ẟ_style), g, keyword("parenR", ẟ_style))
}

export function parenthesiseIf (parens: boolean, g: SVGSVGElement, ẟ_style: DeltaStyle): SVGSVGElement {
   return parens ? parenthesise(g, ẟ_style) : g
}

// TODO: use Point consistently everywhere?
function pointsToString (p̅: [number, number][]): string {
   return p̅.map(([x, y]: [number, number]) => `${round(x)},${round(y)}`).join(" ")
}

export function polyline (p̅: [number, number][], stroke: string, strokeWidth: number): SVGPolylineElement {
   const l: SVGPolylineElement = createElement("polyline", polyline)
   l.setAttribute("points", pointsToString(p̅))
   l.setAttribute("stroke", stroke)
   l.setAttribute("stroke-width", `${round(strokeWidth)}`)
   l.setAttribute("stroke-linecap", "round")
   l.setAttribute("fill", "none")
   return l
}

export function rect (x: number, y: number, width: number, height: number, stroke: string, fill: string, createdBy: Function): SVGRectElement {
   const r: SVGRectElement = createElement("rect", createdBy)
   r.setAttribute("x", `${round(x)}`)
   r.setAttribute("y", `${round(y)}`)
   r.setAttribute("width", `${round(width)}`)
   r.setAttribute("height", `${round(height)}`)
   r.setAttribute("stroke", stroke)
   r.setAttribute("fill", fill)
   return r
}

// Rounding to pixel boundaries (although often desirable for SVG, e.g. to get sharp lines) doesn't work well 
// for small shapes, but we don't need to maintain the full monstrosity that are floating-point numbers. Round 
// to an appropriate number of decimal places, cast to number to strip trailing zeros, and then cast back to string.
// This seems to be sufficient precision for SVG but is also human-friendly.
export function round (n: number): string {
   return (+n.toFixed(3)).toString()
}

// Needs to be at the bottom in the z-order, and opaque.
export function shading (g: SVGSVGElement, fill: string): SVGSVGElement {
   const svg: SVGSVGElement = createElement("svg", shading)
   const { width, height }: Dimensions = dimensions.get(g)!
   const background: SVGRectElement = rect(g.x.baseVal.value, g.y.baseVal.value, width, height, "none", fill, shading)
   background.setAttribute("pointer-events", "none")
   svg.appendChild(background)
   svg.appendChild(g)
   dimensions.set(svg, { width, height })
   return svg
}

export function space (): SVGElement {
   return text(`${space_char}`, DeltaStyle.Unchanged)
}

// Content below or to the left is clipped automatically; content to above or to the right is clipped 
// if we set width and height.
export function svgElement (x: number, y: number, width: number, height: number, defs: boolean, createdBy: Function): SVGSVGElement {
   const svg: SVGSVGElement = createElement("svg", createdBy)
   svg.setAttribute("x", `${round(x)}`)
   svg.setAttribute("y", `${round(y)}`)
   svg.setAttribute("width", `${round(width)}`)
   svg.setAttribute("height", `${round(height)}`)
//   svg.setAttribute("overflow", "visible") // for debugging
   if (defs) {
      const d: SVGDefsElement = createElement("defs", createdBy)
      d.setAttribute("id", "defs")
      svg.appendChild(d)
   }
   return svg
}

// Chrome doesn't appear to fully support SVG 2.0 yet; in particular, transform attributes on svg elements are 
// ignored (except at the root). To invert the y-axis, we have to add a nested g element containing the transform.
export function svgElement_inverted (w: number, h: number): [SVGSVGElement, SVGGElement] {
   const svg: SVGSVGElement = svgElement(0, 0, w, h, true, svgElement_inverted)
   const g: SVGGElement = createElement("g", svgElement_inverted)
   g.setAttribute("transform", `scale(1,-1) translate(0,${-h})`)
   g.setAttribute("width", `${w}`)
   g.setAttribute("height", `${h}`)
   svg.appendChild(g)
   return [svg, g]
}

// Top-level SVG node with a "defs" element with id "defs".
export function svgRootElement (w: number, h: number): SVGSVGElement {
   const svg: SVGSVGElement = svgElement(0, 0, w, h, true, svgRootElement)
   // See https://vecta.io/blog/guide-to-getting-sharp-and-crisp-svg-images
   svg.setAttribute("viewBox", `-0.5 -0.5 ${w.toString()} ${h.toString()}`)
   svg.style.verticalAlign = "top"
   svg.style.display = "inline-block"
   return svg
}

export function text (str: string, ẟ_style: DeltaStyle): SVGTextElement {
   const text: SVGTextElement = textElement(fontSize, [classes, ẟ_style].join(" "), str)
   text.setAttribute("transform", `translate(${0},${lineHeight / 2})`)
   text.setAttribute("alignment-baseline", "central")
   const width: number = svg.textWidth(text)
   dimensions.set(text, { width, height: lineHeight })
   text.remove()
   return text
}

function textElement (fontSize: number, class_: string, str: string): SVGTextElement {
   const text: SVGTextElement = createElement("text", textElement)
   text.setAttribute("stroke", "none")
   text.setAttribute("font-size", fontSize.toString()) // wasn't able to set this through CSS for some reason
   text.setAttribute("class", class_) // set styling before creating text node, for font metrics to be correct
   text.appendChild(document.createTextNode(str))
   return text
}

// Flip text vertically to cancel out the global vertical flip. Don't set x and y but express
// position through a translation so that the scaling doesn't affect the position.
export function textElement_graphical (x: number, y: number, fontSize: number, str: string): SVGTextElement {
   const text: SVGTextElement = textElement(fontSize, "label", str)
   let transform: string = `translate(${round(x)},${round(y)})`
   text.setAttribute("transform", transform + " scale(1,-1)")
   return text
}

export function unimplemented (v: Value): SVGSVGElement {
   return horiz(text(`TODO: ${className(v)}`, DeltaStyle.Unchanged))
}

export function vert (...gs: SVGElement[]): SVGSVGElement {
   const g: SVGSVGElement = createElement("svg", vert)
   let height_sum: number = 0,
       width_max: number = 0
   gs.forEach((gʹ: SVGElement): void => {
      gʹ.setAttribute("y", `${height_sum}`)
      gʹ.setAttribute("x", `0`)
      const { width, height }: Dimensions = dimensions.get(gʹ)!
      height_sum += height
      width_max = Math.max(width_max, width)
      g.appendChild(gʹ)
   })
   dimensions.set(g, { width: width_max, height: height_sum })
   return g
}

export enum DeltaStyle {
   New = "new",
   Changed = "changed",
   Unchanged = "unchanged"
}

// Delta-styling for the constructor component of a value (not its child pointers). In particular, primitives appear changed
// iff their value has changed, whereas non-primitives appear changed iff reclassified. Changes to child pointers must be
// visualised separately.
export function deltaStyle (v: Value): DeltaStyle {
   if (versioned(v)) {
      if (v.__ẟ instanceof New) {
         return DeltaStyle.New
      } else
      if (v.__ẟ instanceof Change) {
         if (Object.keys(v.__ẟ.changed).length > 0 && isPrim(v)) {
            return DeltaStyle.Changed
         } else {
            return DeltaStyle.Unchanged
         }
      } else
      if (v.__ẟ instanceof Reclassify) {
         return DeltaStyle.Changed
      } else {
         return absurd()
      }
   } else {
      return absurd()
   }
} 
