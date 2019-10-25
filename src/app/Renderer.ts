import { __nonNull, absurd, as, className } from "../util/Core"
import { Change, New, Reclassify } from "../Delta"
import { strings } from "../Expr"
import { Value, isPrim } from "../Value"
import { versioned } from "../Versioned"
import { SVG } from "./Core"
import "./styles.css"

// Maybe there's some built-in type for this, but don't care yet
type Rect = { x: number, y: number, width: number, height: number }

export const svg: SVG = new SVG(false)
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

export function arrowhead (): SVGMarkerElement {
   const marker: SVGMarkerElement = document.createElementNS(SVG.NS, "marker")
   marker.setAttribute("id", "arrowhead")
//   marker.setAttribute("viewBox", "0 0 10 10")
   marker.setAttribute("refX", "left")
   marker.setAttribute("refY", "2") // vertical midpoint of arrowhead
   marker.setAttribute("markerUnits", "strokeWidth")
   marker.setAttribute("markerWidth", "16")
   marker.setAttribute("markerHeight", "16")
   marker.setAttribute("orient", "auto")
   marker.setAttribute("fill", "blue") // will want to change this
   const path: SVGPathElement = document.createElementNS(SVG.NS, "path")
   marker.appendChild(path)
   path.setAttribute("d", "M 0 0 L 6 4 L 0 4 Z") // half arrowhead
   return marker
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

export function connector (g1: SVGSVGElement, g2: SVGSVGElement): SVGElement {
   const connector: SVGLineElement = document.createElementNS(SVG.NS, "line")
   const g1_: Rect = rect(g1)
   const g2_: Rect = rect(g2)
   if (leftOf(g1_, g2_)) {
      connector.setAttribute("x1", `${g1_.x + g1_.width}`)
      connector.setAttribute("y1", `${g1_.y}`)
      connector.setAttribute("x2", `${g2_.x}`)
      connector.setAttribute("y2", `${g2_.y}`)
   } else {
      connector.setAttribute("x1", `${g1_.x}`)
      connector.setAttribute("y1", `${g1_.y + g1_.height}`)
      connector.setAttribute("x2", `${g2_.x + g2_.width}`)
      connector.setAttribute("y2", `${g2_.y + g2_.height}`)   
   }
   connector.setAttribute("stroke", "blue") // hardcoded
   connector.setAttribute("stroke-width", "1")
   connector.setAttribute("stroke-dasharray", "1,1")
   connector.setAttribute("marker-end", "url(#arrowhead)")
   return connector
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
