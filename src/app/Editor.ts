import { absurd, as, className, error, log } from "../util/Core"
import { Cons, Nil } from "../BaseTypes"
import { Expr } from "../Expr"
import { openWithImports } from "../Module"
import { createSvg, svgMetrics, svgNS, textElement, textHeight } from "./Core"
import "./styles.css"

import Trie = Expr.Trie

const fontSize: number = 18,
      class_: string = "code",
      // bizarrely, if I do this later, font metrics are borked:
      lineHeight = log(Math.ceil(textHeight(fontSize, class_, "m")) * 2), // representative character 
      // ASCII spaces seem to be trimmed; only Unicode space that seems to render monospaced is this: 
      space: string = "\u00a0"

class Renderer {
   x: number
   line: number

   constructor () {
      this.x = 0
      this.line = 0
   }

   // Post-condition: returned element has an entry in "dimensions" map. 
   render (e: Expr): SVGElement {
      if (e instanceof Expr.ConstNum) {
         return this.renderText(e.val.toString())
      } else
      if (e instanceof Expr.ConstStr) {
         return this.renderText(e.val.toString())
      } else
      if (e instanceof Expr.DataExpr) {
         if (e.ctr === Nil.name || e.ctr === Cons.name) {
            return Renderer.group(this.renderText("["), ...this.renderElements(e), this.renderText("]"))
         } else {
            return this.renderText(`<${className(e)}>`)
         }
      } else
      if (e instanceof Expr.Var) {
         return this.renderText(e.x.val)
      } else
      if (e instanceof Expr.Fun) {
         return this.renderTrie(e.σ)
      } else
      if (e instanceof Expr.App) {
         return Renderer.group(...this.renderHoriz(e.f, e.e))
      } else {
         return this.renderText(`<${className(e)}>`)
      }
   }

   // Expressions for the elements, plus expression for tail (or null if list terminates with nil).
   listElements (e: Expr): [Expr[], Expr | null] {
      if (e instanceof Expr.DataExpr) {
         if (e.ctr === Nil.name) {
            return [[], null]
         } else
         if (e.ctr === Cons.name) {
            // use cursor interface instead?
            const [es, eʹ]: [Expr[], Expr | null] = this.listElements(as(e.__child("tail"), Expr.Expr))
            return [[as(e.__child("head"), Expr.Expr), ...es], eʹ]
         } else {
            return error(`Found ${e.ctr}, expected list.`)
         }
      } else {
         return [[], e]
      }
   }

   renderElements (e: Expr): SVGElement[] {
      const [es, eʹ]: [Expr[], Expr | null] = this.listElements(e),
            vs: SVGElement[] = []
      es.forEach((e: Expr, n: number): void => {
         vs.push(this.render(e))
         if (n < es.length - 1) {
            vs.push(this.renderText(`,${space}`))
         }
      })
      if (eʹ !== null) {
         vs.push(this.renderText(", ..."), this.render(eʹ))
      }
      return vs
   }

   renderTrie (σ: Trie<Expr>): SVGElement {
      if (Trie.Var.is(σ)) {
         return this.renderText(σ.x.val)
      } else
      if (Trie.Constr.is(σ)) {
         return this.renderText(`<${className(σ)}>`)
      } else {
         return absurd()
      }
   }

   renderHoriz (...es: Expr[]): SVGElement[] {
      const vs: SVGElement[] = []
      es.forEach((e: Expr, n: number): void => {
         vs.push(this.render(e))
         if (n < es.length - 1) {
            // ASCII spaces seem to be trimmed; only Unicode space that seems to render monospaced is this: 
            vs.push(this.renderText(`${space}`))
         }
      })
      return vs
   }

   static group (...vs: SVGElement[]): SVGElement {
      const g: SVGGElement = document.createElementNS(svgNS, "g")
      let width_sum: number = 0,
          height_max: number = 0
      g.setAttribute("pointer-events", "bounding-box")
      vs.forEach((v: SVGElement): void => {
         const { width, height }: Dimensions = dimensions.get(v)!
         width_sum += width
         height_max = Math.max(height_max, height)
         g.appendChild(v)
      })
      dimensions.set(g, { width: width_sum, height: height_max })
      return g
   }
   
   renderText (str: string): SVGTextElement {
      const text: SVGTextElement = textElement(this.x, this.line * lineHeight, fontSize, class_, str)
      svgMetrics!.appendChild(text)
      const { width } = text.getBBox()
      dimensions.set(text, { width, height: lineHeight })
      text.remove()
      this.x += width
      return text
   }
}

// Populate explicity, rather than using a memoised function.
type Dimensions = { width: number, height: number }
const dimensions: Map<SVGElement, Dimensions> = new Map()

class Editor {
   constructor () {
      // Wait for fonts to load before rendering, otherwise metrics will be wrong.
      window.onload = (ev: Event): void => {
         const root: SVGSVGElement = createSvg(400, 400, false)
         document.body.appendChild(root)
         const e: Expr = as(openWithImports("foldr_sumSquares"), Expr.Defs).e
         root.appendChild(new Renderer().render(e))
      }
   }
}

new Editor()
