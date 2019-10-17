import { absurd, className } from "../util/Core"
import { Change, New, Reclassify } from "../Delta"
import { strings } from "../Expr"
import { Value, isPrim } from "../Value"
import { versioned } from "../Versioned"
import { SVG } from "./Core"
import "./styles.css"

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

export function border (g: SVGSVGElement): SVGElement {
   const border: SVGRectElement = document.createElementNS(SVG.NS, "rect")
   border.setAttribute("x", g.x.baseVal.valueAsString)
   border.setAttribute("y", g.y.baseVal.valueAsString)
   const { width, height }: Dimensions = dimensions.get(g)!
   border.setAttribute("height", height.toString())
   border.setAttribute("width", width.toString())
   border.setAttribute("stroke", "gray")
   border.setAttribute("stroke-dasharray", "1,1")
   border.setAttribute("fill", "none")
   g.appendChild(border)
   return g
}

export function bracket (gs: SVGElement[], ẟ_style: DeltaStyle): SVGElement {
   return horiz(keyword("bracketL", ẟ_style), ...gs, keyword("bracketR", ẟ_style))
}

export function comma (ẟ_style: DeltaStyle): SVGElement {
   return keyword("comma", ẟ_style)
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

export function ellipsis (ẟ_style: DeltaStyle): SVGElement {
   return keyword("ellipsis", ẟ_style)
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

export function parenthesise (g: SVGElement, ẟ_style: DeltaStyle): SVGElement {
   return horiz(keyword("parenL", ẟ_style), g, keyword("parenR", ẟ_style))
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

export function unimplemented (v: Value): SVGElement {
   return text(`TODO: ${className(v)}`, DeltaStyle.Unchanged)
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
