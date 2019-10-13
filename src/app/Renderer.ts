import { zip } from "../util/Array"
import { absurd, as, className, error } from "../util/Core"
import { Cons, List, Nil } from "../BaseTypes"
import { exprClass } from "../DataType"
import { Change, New, Reclassify, __deltas } from "../Delta"
import { Expr, strings } from "../Expr"
import { DataElim, Elim, VarElim } from "../Match"
import { Num, Str, Value, fields } from "../Value"
import { versioned } from "../Versioned"
import { SVG } from "./Core"
import { ExprCursor } from "./Cursor"
import "./styles.css"

import Cont = Expr.Cont

export const svg: SVG = new SVG(false)
const fontSize: number = 18
const classes: string = "code"
// bizarrely, if I do this later, font metrics are borked:
const lineHeight = svg.textHeight(textElement(0, 0, fontSize, classes, "m")) // representative character 
// ASCII spaces seem to be trimmed; only Unicode space that seems to render monospaced is this: 
const space: string = "\u00a0"

// Populate explicity, rather than using a memoised function.
type Dimensions = { width: number, height: number }
const dimensions: Map<SVGElement, Dimensions> = new Map()

function textElement (x: number, y: number, fontSize: number, class_: string, str: string): SVGTextElement {
   const text: SVGTextElement = document.createElementNS(SVG.NS, "text")
   text.setAttribute("font-size", fontSize.toString()) // wasn't able to set this through CSS for some reason
   text.setAttribute("class", class_) // set styling before creating text node, for font metrics to be correct
   text.appendChild(document.createTextNode(str))
   return text
}

export interface EditListener {
   onEdit (): void
}

export class Renderer {
   editor: EditListener

   constructor (editor: EditListener) {
      this.editor = editor
   }

   prompt (e: Expr, v: Value): SVGElement {
      return Renderer.vert(
         this.expr(e),
         Renderer.horiz(
            this.text(">"),
            this.space(), this.value(v)
         )
      )
   }

   value (v: Value): SVGElement {
      if (v instanceof Num) {
         return this.num(v, false)
      } else
      if (v instanceof Str) {
         return this.text(v.val.toString(), deltaStyle(v))
      } else 
      if (v instanceof List) {
         return Renderer.horiz(this.text("["), ...this.elements([v.toArray(), null]), this.text("]"))
      } else {
         return this.text(`<${className(v)}>`)
      }
   }

   exprOrValue (v: Value): SVGElement {
      if (v instanceof Expr.Expr) {
         return this.expr(v)
      } else {
         return this.value(v)
      }
   }

   // Post-condition: returned element has an entry in "dimensions" map. 
   expr (e: Expr): SVGElement {
      if (e instanceof Expr.ConstNum) {
         return this.num(e.val, true)
      } else
      if (e instanceof Expr.ConstStr) {
         return this.text(e.val.toString())
      } else
      if (e instanceof Expr.DataExpr) {
         if (className(e) === exprClass(Nil.name).name || className(e) === exprClass(Cons.name).name) {
            return Renderer.horiz(this.text("["), ...this.elements(elements_expr(e)), this.text("]"))
         } else {
            return this.text(`<${className(e)}>`)
         }
      } else
      if (e instanceof Expr.Var) {
         return this.text(e.x.val)
      } else
      if (e instanceof Expr.Fun) {
         return this.elim(e.σ)
      } else
      if (e instanceof Expr.BinaryApp) {
         return Renderer.horiz(this.expr(e.e1), this.space(), this.text(e.opName.val), this.space(), this.expr(e.e2))
      } else
      if (e instanceof Expr.App) {
         return Renderer.horiz(
            e.f instanceof Expr.Fun ? this.parenthesise(e.f) : this.expr(e.f), 
            this.space(), 
            e.e instanceof Expr.Fun ? this.parenthesise(e.e) : this.expr(e.e)
         )
      } else
      if (e instanceof Expr.Defs) {
         return Renderer.vert(
            this.text(`<${className(e)}>`), 
            this.expr(e.e)
         )
      } else {
         return absurd()
      }
   }

   parenthesise (e: Expr): SVGElement {
      return Renderer.horiz(
         this.text("("),
         this.expr(e),
         this.text(")")
      )
   }

   num (n: Num, editable: boolean): SVGElement {
      const v: SVGElement = this.text(n.toString(), deltaStyle(n))
      if (editable && Number.isInteger(n.val)) {
         v.addEventListener("click", (ev: MouseEvent): void => {
            new ExprCursor(n).setNum(n.val + 1)
            ev.stopPropagation()
            this.editor.onEdit()
         })
      }
      return v
   }

   space (): SVGElement {
      return this.text(`${space}`)
   }

   elements ([es, eʹ]: [Value[], Value | null]): SVGElement[] {
      const vs: SVGElement[] = []
      es.forEach((e: Value, n: number): void => {
         vs.push(this.exprOrValue(e))
         if (n < es.length - 1) {
            vs.push(this.text(","), this.space())
         }
      })
      if (eʹ !== null) {
         vs.push(this.text(", ..."), this.exprOrValue(eʹ))
      }
      return vs
   }

   elim<K extends Cont> (σ: Elim<K>): SVGElement {
      if (VarElim.is(σ)) {
         return Renderer.horiz(
            this.text(σ.x.val),
            this.space(), this.text(strings.arrow), 
            this.space(), this.cont(σ.κ)
         )
      } else
      if (DataElim.is(σ)) {
         return Renderer.vert(
            ...zip(fields(σ), σ.__children as Cont[]).map(([ctr, κ]) => {
               return Renderer.horiz(
                  this.text(ctr),
                  this.space(),
                  this.text(strings.arrow),
                  this.space(),
                  this.cont(κ)
               )
            })
         )
      } else {
         return absurd()
      }
   }

   cont (κ: Cont): SVGElement {
      if (κ instanceof Expr.Expr) {
         return this.expr(κ)
      } else
      if (κ instanceof Elim) {
         return this.elim(κ)
      } else {
         return absurd()
      }
   }

   // TODO: completely broken; ignores the fact that elements have x, y coordinates :-/
   static old_group (...vs: SVGElement[]): SVGElement {
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

   static horiz (...gs: SVGElement[]): SVGElement {
      const g: SVGGElement = document.createElementNS(SVG.NS, "svg")
      let width_sum: number = 0,
          height_max: number = 0
      gs.forEach((gʹ: SVGElement): void => {
         gʹ.setAttribute("x", `${width_sum}`)
         gʹ.setAttribute("y", `${lineHeight}`)
         gʹ.removeAttribute("transform") // don't use transform any more
         const { width, height }: Dimensions = dimensions.get(gʹ)!
         width_sum += width
         height_max = Math.max(height_max, height)
         g.appendChild(gʹ)
      })
      dimensions.set(g, { width: width_sum, height: height_max })
      return g
   }

   static vert (...gs: SVGElement[]): SVGElement {
      const g: SVGGElement = document.createElementNS(SVG.NS, "svg")
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

   text (str: string, ẟ_style?: string): SVGTextElement {
      ẟ_style = ẟ_style || "unchanged" // default
      const text: SVGTextElement = textElement(0, 0, fontSize, [classes, ẟ_style].join(" "), str)
      text.setAttribute("transform", `translate(${0},${lineHeight})`)
      const width: number = svg.textWidth(text)
      dimensions.set(text, { width, height: lineHeight })
      text.remove()
      return text
   }
}

function deltaStyle (v: Value): string{
   if (versioned(v)) {
      if (v.__ẟ instanceof New) {
         return "new"
      } else
      if (v.__ẟ instanceof Change) {
         if (Object.keys(v.__ẟ.changed).length === 0) {
            return "unchanged"
         } else {
            return "changed"
         }
      } else
      if (v.__ẟ instanceof Reclassify) {
         return "changed"
      } else {
         return absurd()
      }
   } else {
      return absurd()
   }
} 

// Expressions for the elements, plus expression for tail (or null if list terminates with nil).
function elements_expr (e: Expr): [Expr[], Expr | null] {
   if (e instanceof Expr.DataExpr) {
      if (className(e) === exprClass(Nil.name).name) {
         return [[], null]
      } else
      if (className(e) === exprClass(Cons.name).name) {
         // use cursor interface instead?
         const [es, eʹ]: [Expr[], Expr | null] = elements_expr(as(e.__child("tail"), Expr.Expr))
         return [[as(e.__child("head"), Expr.Expr), ...es], eʹ]
      } else {
         return error(`Found ${e.ctr}, expected list.`)
      }
   } else {
      return [[], e]
   }
}
