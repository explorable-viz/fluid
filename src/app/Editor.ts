import { zip } from "../util/Array"
import { absurd, as, className, error, log } from "../util/Core"
import { Cons, Nil } from "../BaseTypes"
import { exprClass } from "../DataType"
import { ExplValue } from "../DataValue"
import { __deltas } from "../Delta"
import { emptyEnv } from "../Env"
import { Eval } from "../Eval"
import { Expr, strings } from "../Expr"
import { DataElim, Elim, VarElim } from "../Match"
import { openWithImports } from "../Module"
import { Num, Str, Value, fields } from "../Value"
import { SVG } from "./Core"
import { ExprCursor } from "./Cursor"
import "./styles.css"

import Cont = Expr.Cont

const svg: SVG = new SVG(false),
      fontSize: number = 18,
      class_: string = "code",
      // bizarrely, if I do this later, font metrics are borked:
      lineHeight = log(Math.ceil(svg.textHeight(fontSize, class_, "m")) * 2), // representative character 
      // ASCII spaces seem to be trimmed; only Unicode space that seems to render monospaced is this: 
      space: string = "\u00a0"

class Renderer {
   x: number
   line: number

   constructor () {
      this.x = 0
      this.line = 1
   }

   renderPrompt(e: Expr, v: Value): SVGElement {
      const e_g: SVGElement = this.render(e)
      this.line++
      return Renderer.group(
         e_g,
         Renderer.group(
            this.renderText(">"),
            this.space(), this.renderValue(v)
         )
      )
   }

   renderValue (v: Value): SVGElement {
      if (v instanceof Num) {
         return this.renderNum(v, false)
      } else
      if (v instanceof Str) {
         return this.renderText(v.val.toString())
      } else {
         return this.renderText(`<${className(v)}>`)
      }
   }

   // Post-condition: returned element has an entry in "dimensions" map. 
   render (e: Expr): SVGElement {
      if (e instanceof Expr.ConstNum) {
         return this.renderNum(e.val, true)
      } else
      if (e instanceof Expr.ConstStr) {
         return this.renderText(e.val.toString())
      } else
      if (e instanceof Expr.DataExpr) {
         if (className(e) === exprClass(Nil.name).name || className(e) === exprClass(Cons.name).name) {
            return Renderer.group(this.renderText("["), ...this.renderElements(e), this.renderText("]"))
         } else {
            return this.renderText(`<${className(e)}>`)
         }
      } else
      if (e instanceof Expr.Var) {
         return this.renderText(e.x.val)
      } else
      if (e instanceof Expr.Fun) {
         return this.renderElim(e.σ)
      } else
      if (e instanceof Expr.BinaryApp) {
         return Renderer.group(this.render(e.e1), this.space(), this.renderText(e.opName.val), this.space(), this.render(e.e2))
      } else
      if (e instanceof Expr.App) {
         const g_f: SVGElement = e.f instanceof Expr.Fun ? this.renderParens(e.f) : this.render(e.f),
               g_e: SVGElement = e.e instanceof Expr.Fun ? this.renderParens(e.e) : this.render(e.e)
         return Renderer.group(g_f, this.space(), g_e)
      } else {
         return absurd()
      }
   }

   renderParens (e: Expr): SVGElement {
      return Renderer.group(
         this.renderText("("),
         this.render(e),
         this.renderText(")")
      )
   }

   renderNum (n: Num, editable: boolean): SVGElement {
      const v: SVGElement = this.renderText(n.toString())
      if (editable && Number.isInteger(n.val)) {
         v.addEventListener("click", (ev: MouseEvent): void => {
            new ExprCursor(n).setNum(n.val + 1)
            ev.stopPropagation()
         })
      }
      return v
   }

   space (): SVGElement {
      return this.renderText(`${space}`)
   }

   renderElements (e: Expr): SVGElement[] {
      const [es, eʹ]: [Expr[], Expr | null] = listElements(e),
            vs: SVGElement[] = []
      es.forEach((e: Expr, n: number): void => {
         vs.push(this.render(e))
         if (n < es.length - 1) {
            vs.push(this.renderText(","), this.space())
         }
      })
      if (eʹ !== null) {
         vs.push(this.renderText(", ..."), this.render(eʹ))
      }
      return vs
   }

   renderElim<K extends Cont> (σ: Elim<K>): SVGElement {
      if (VarElim.is(σ)) {
         return Renderer.group(
            this.renderText(σ.x.val),
            this.space(), this.renderText(strings.arrow), 
            this.space(), this.renderCont(σ.κ)
         )
      } else
      if (DataElim.is(σ)) {
         return Renderer.group(
            ...zip(fields(σ), σ.__children as Cont[]).map(([ctr, κ]) => {
               return Renderer.group(
                  this.renderText(ctr),
                  this.space(),
                  this.renderText(strings.arrow),
                  this.space(),
                  this.renderCont(κ)
               )
            })
         )
      } else {
         return absurd()
      }
   }

   renderCont (κ: Cont): SVGElement {
      if (κ instanceof Expr.Expr) {
         return this.render(κ)
      } else
      if (κ instanceof Elim) {
         return this.renderElim(κ)
      } else {
         return absurd()
      }
   }

   renderHoriz (...es: Expr[]): SVGElement[] {
      const vs: SVGElement[] = []
      es.forEach((e: Expr, n: number): void => {
         vs.push(this.render(e))
         if (n < es.length - 1) {
            vs.push(this.renderText(`${space}`))
         }
      })
      return vs
   }

   // TODO: completely broken; ignores the fact that elements have x, y coordinates :-/
   static group (...vs: SVGElement[]): SVGElement {
      const g: SVGGElement = document.createElementNS(SVG.NS, "g")
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
      const text: SVGTextElement = svg.textElement(this.x, this.line * lineHeight, fontSize, class_, str)
      svg.metrics!.appendChild(text)
      const { width } = text.getBBox()
      dimensions.set(text, { width, height: lineHeight })
      text.remove()
      this.x += width
      return text
   }
}

// Expressions for the elements, plus expression for tail (or null if list terminates with nil).
function listElements (e: Expr): [Expr[], Expr | null] {
   if (e instanceof Expr.DataExpr) {
      if (className(e) === exprClass(Nil.name).name) {
         return [[], null]
      } else
      if (className(e) === exprClass(Cons.name).name) {
         // use cursor interface instead?
         const [es, eʹ]: [Expr[], Expr | null] = listElements(as(e.__child("tail"), Expr.Expr))
         return [[as(e.__child("head"), Expr.Expr), ...es], eʹ]
      } else {
         return error(`Found ${e.ctr}, expected list.`)
      }
   } else {
      return [[], e]
   }
}

// Populate explicity, rather than using a memoised function.
type Dimensions = { width: number, height: number }
const dimensions: Map<SVGElement, Dimensions> = new Map()

class Editor {
   constructor () {
      // Wait for fonts to load before rendering, otherwise metrics will be wrong.
      window.onload = (ev: Event): void => {
         const root: SVGSVGElement = svg.createSvg(800, 400)
         document.body.appendChild(root)
         const e0: Expr = openWithImports("foldr_sumSquares"),
               e: Expr = as(e0, Expr.Defs).e,
               tv: ExplValue = Eval.eval_(emptyEnv(), e0)
         __deltas.clear()         
         root.appendChild(new Renderer().renderPrompt(e, tv.v))
      }
   }
}

new Editor()
