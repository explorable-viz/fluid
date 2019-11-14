import { __nonNull, absurd, as, className } from "../util/Core"
import { Change, New, Reclassify } from "../Delta"
import { strings } from "../Expr"
import { Value, isPrim } from "../Value"
import { versioned } from "../Versioned"
import { SVG } from "./Core"
import "./styles.css"

// Maybe there are some built-in types for this, but don't care yet
type Point = { x: number, y: number }
type Rect = Point & { width: number, height: number }

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

export function border (g: SVGSVGElement, stroke: string): SVGRectElement {
   const border: SVGRectElement = document.createElementNS(SVG.NS, "rect")
   border.setAttribute("x", g.x.baseVal.valueAsString)
   border.setAttribute("y", g.y.baseVal.valueAsString)
   const { width, height }: Dimensions = dimensions.get(g)!
   border.setAttribute("height", height.toString())
   border.setAttribute("width", width.toString())
   border.setAttribute("stroke", stroke)
   border.setAttribute("fill", "none")
   return border
}

export function border_changed (g: SVGSVGElement): SVGSVGElement {
   const border_: SVGRectElement = border(g, "blue")
   border_.setAttribute("stroke-dasharray", "1,1")
   g.appendChild(border_)
   return g
}

export function border_focus (g: SVGSVGElement): SVGSVGElement {
   const border_: SVGRectElement = border(g, "gray")
   g.appendChild(border_)
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
function leftOf (r1: Rect, r2: Rect): boolean {
   return r1.x + r1.width / 2 <= r2.x + r2.width
}

function blah (x: number, length: number, proportion: number):  number {
   return x + proportion * length
}

// Path segment corrresponding to a line.
export function line (p1: Point, p2: Point): string {
   return `M ${p1.x} ${p1.y} L ${p2.x} ${p2.y}`
}

// Offset might be better computed as a function of distance between p1 and p2.
function curvedLine (p1: Point, p2: Point, offset: number): string {
   const mp: Point = { x: (p2.x + p1.x) * 0.5, y: (p2.y + p1.y) * 0.5 }
   // angle of perpendicular to line
   const theta = Math.atan2(p2.y - p1.y, p2.x - p1.x) - Math.PI / 2
   const control: Point = { x: mp.x + offset * Math.cos(theta), y: mp.y + offset * Math.sin(theta) }
   return `M ${p1.x} ${p1.y} Q ${control.x} ${control.y} ${p2.x} ${p2.y}`
}

export function connector (g1: SVGSVGElement, g2: SVGSVGElement): SVGElement {
   const g1_: Rect = rect(g1)
   const g2_: Rect = rect(g2)
   const [fromBottom, fromTop]: [number, number] = [0.1, 0.9]
   const connector_: SVGPathElement = document.createElementNS(SVG.NS, "path")
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
   connector_.setAttribute("marker-end", "url(#arrowhead)")
   return connector_
}

// Assume root has a unique defs element called "defs".
export function defineMarker (root: SVGSVGElement, marker: SVGMarkerElement): void {
   const defs: SVGDefsElement = as(root.getElementById("defs"), SVGDefsElement)
   defs.appendChild(marker)
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

export function edge_left (g: SVGSVGElement): SVGSVGElement {
   const edge: SVGLineElement = document.createElementNS(SVG.NS, "line")
   edge.setAttribute("x1", g.x.baseVal.valueAsString)
   edge.setAttribute("y1", g.y.baseVal.valueAsString)
   const { height }: Dimensions = dimensions.get(g)!
   edge.setAttribute("x2", g.x.baseVal.valueAsString)
   edge.setAttribute("y2", `${g.y.baseVal.value + height}`)
   edge.setAttribute("stroke", "gray")
   edge.setAttribute("stroke-width", "4")
//   edge.setAttribute("stroke-dasharray", "2,2")
   g.appendChild(edge)
   return g
}

export function edge_bottom (g: SVGSVGElement): SVGSVGElement {
   const edge: SVGLineElement = document.createElementNS(SVG.NS, "line")
   const { width, height }: Dimensions = dimensions.get(g)!
   edge.setAttribute("x1", g.x.baseVal.valueAsString)
   edge.setAttribute("y1", `${g.y.baseVal.value + height}`)
   edge.setAttribute("x2", `${g.y.baseVal.value + width}`)
   edge.setAttribute("y2", `${g.y.baseVal.value + height}`)
   edge.setAttribute("stroke", "gray")
   edge.setAttribute("stroke-width", "2")
//   edge.setAttribute("stroke-dasharray", "2,2")
   g.appendChild(edge)
   return g
}

export function ellipsis (ẟ_style: DeltaStyle): SVGElement {
   return text("…", ẟ_style)
}

export function horiz (...gs: SVGElement[]): SVGSVGElement {
   const g: SVGSVGElement = document.createElementNS(SVG.NS, "svg")
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

export function marker (id: string, fill: string): SVGMarkerElement {
   const m: SVGMarkerElement = document.createElementNS(SVG.NS, "marker")
   m.setAttribute("id", id)
   m.setAttribute("markerUnits", "strokeWidth")
   m.setAttribute("orient", "auto")
   m.setAttribute("fill", fill)
   return m
}

export function marker_arrowhead (): SVGMarkerElement {
   const m: SVGMarkerElement = marker("arrowhead", "blue")
//   marker.setAttribute("viewBox", "0 0 10 10")
   const length: number = 6,
         width: number = 4
   m.setAttribute("refX", `${length}`)
   m.setAttribute("refY", `${width / 2}`)
   m.setAttribute("markerWidth", "16")
   m.setAttribute("markerHeight", "16")
   const path: SVGPathElement = document.createElementNS(SVG.NS, "path")
   m.appendChild(path)
   path.setAttribute("d", `M ${length} ${width / 2} L 0 ${width} L 0 0 Z`)
   return m
}

export function marker_tick (): SVGMarkerElement {
   const m: SVGMarkerElement = marker("tick", "black")
   const length: number = 6,
         width: number = 4
   m.setAttribute("refX", `${length}`)
   m.setAttribute("refY", `${width / 2}`)
   m.setAttribute("markerWidth", "16")
   m.setAttribute("markerHeight", "16")
   const line: SVGLineElement = document.createElementNS(SVG.NS, "line")
   m.appendChild(line)
   line.setAttribute("x1", `${0}`)
   line.setAttribute("y1", `${0}`)
   line.setAttribute("x1", `${0}`)
   line.setAttribute("y1", `${5}`)
   line.setAttribute("stroke", "black")
   return m
}

export function parenthesise (g: SVGElement, ẟ_style: DeltaStyle): SVGSVGElement {
   return horiz(keyword("parenL", ẟ_style), g, keyword("parenR", ẟ_style))
}

export function parenthesiseIf (parens: boolean, g: SVGSVGElement, ẟ_style: DeltaStyle): SVGSVGElement {
   return parens ? parenthesise(g, ẟ_style) : g
}

function rect (g: SVGSVGElement): Rect {
   const { width, height }: Dimensions = __nonNull(dimensions.get(g))
   const { x, y } = coordinates(g)
   return { x, y, width, height }
}

// Needs to be at the bottom in the z-order, and opaque.
export function shading (g: SVGSVGElement, fill: string): SVGSVGElement {
   const svg: SVGSVGElement = document.createElementNS(SVG.NS, "svg")
   const background: SVGRectElement = document.createElementNS(SVG.NS, "rect")
   background.setAttribute("x", g.x.baseVal.valueAsString)
   background.setAttribute("y", g.y.baseVal.valueAsString)
   const { width, height }: Dimensions = dimensions.get(g)!
   background.setAttribute("height", height.toString())
   background.setAttribute("width", width.toString())
   background.setAttribute("stroke", "none")
   background.setAttribute("fill", fill)
   background.setAttribute("pointer-events", "none")
   svg.appendChild(background)
   svg.appendChild(g)
   dimensions.set(svg, { width, height })
   return svg
}

export function space (): SVGElement {
   return text(`${space_char}`, DeltaStyle.Unchanged)
}

// Chrome doesn't appear to fully support SVG 2.0 yet; in particular, transform attributes on svg elements are 
// ignored (except at the root). To invert the y-axis, we have to add a nested g element containing the transform.
// Elsewhere we avoid SVG transforms to avoid non-integer pixel attributes.
export function svgElement (w: number, h: number): [SVGSVGElement, SVGGElement] {
   const svg: SVGSVGElement = document.createElementNS(SVG.NS, "svg")
   svg.setAttribute("width", `${w}`)
   svg.setAttribute("height", `${h}`)
   const g: SVGGElement = document.createElementNS(SVG.NS, "g")
   g.setAttribute("transform", `scale(1,-1) translate(0,${-h})`)
   g.setAttribute("width", `${w}`)
   g.setAttribute("height", `${h}`)
   svg.appendChild(g)
   return [svg, g]
}

// Top-level SVG node with a "defs" element with id "defs".
export function svgRootElement (w: number, h: number): SVGSVGElement {
   const svg: SVGSVGElement = document.createElementNS(SVG.NS, "svg")
   svg.setAttribute("width", `${w}`)
   svg.setAttribute("height", `${h}`)
   // See https://vecta.io/blog/guide-to-getting-sharp-and-crisp-svg-images
   svg.setAttribute("viewBox", `-0.5 -0.5 ${w.toString()} ${h.toString()}`)
   svg.style.verticalAlign = "top"
   svg.style.display = "inline-block"
   const defs: SVGDefsElement = document.createElementNS(SVG.NS, "defs")
   defs.setAttribute("id", "defs")
   svg.appendChild(defs)
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
   const text: SVGTextElement = document.createElementNS(SVG.NS, "text")
   text.setAttribute("font-size", fontSize.toString()) // wasn't able to set this through CSS for some reason
   text.setAttribute("class", class_) // set styling before creating text node, for font metrics to be correct
   text.appendChild(document.createTextNode(str))
   return text
}

export function unimplemented (v: Value): SVGSVGElement {
   return horiz(text(`TODO: ${className(v)}`, DeltaStyle.Unchanged))
}

export function vert (...gs: SVGElement[]): SVGSVGElement {
   const g: SVGSVGElement = document.createElementNS(SVG.NS, "svg")
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
