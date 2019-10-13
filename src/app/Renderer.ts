import { flatten, zip } from "../util/Array"
import { Class, __nonNull, absurd, as, assert, className, error } from "../util/Core"
import { Cons, List, Nil, Pair } from "../BaseTypes"
import { arity, exprClass } from "../DataType"
import { Change, New, Reclassify, __deltas } from "../Delta"
import { Expr, strings } from "../Expr"
import { DataElim, Elim, VarElim } from "../Match"
import { Num, Str, Value, fields } from "../Value"
import { ν, at, str, versioned } from "../Versioned"
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

function hasExprClass (e: Expr, C: Class): boolean {
   return className(e) === exprClass(C.name).name
}

export interface EditListener {
   onEdit (): void
}

export class Renderer {
   editor: EditListener

   constructor (editor: EditListener) {
      this.editor = editor
   }

   clauses<K extends Cont> (σ: Elim<K>): [SVGElement[], SVGElement][] {
      if (VarElim.is(σ)) {
         const cs: [SVGElement[], SVGElement][] = this.cont(σ.κ)
         return cs.map(([gs, g]) => [[this.text(σ.x.val), ...gs], g])
      } else
      if (DataElim.is(σ)) {
         const cκs: [string, Cont][] = zip(fields(σ), σ.__children as Cont[])
         return flatten(cκs.filter(([c, κ]) => κ !== undefined).map(([c, κ]): [SVGElement[], SVGElement][] => {
            return this.cont(__nonNull(κ)).map(([gs, g]: [SVGElement[], SVGElement]) => {
               assert(gs.length >= arity(c))
               const ctr_g: SVGElement = this.horizSpace(this.text(c), ...gs.slice(0, arity(c)))
               return [[arity(c) === 0 ? ctr_g : this.parenthesise(ctr_g), ...gs.slice(arity(c))], g]
            })
         }))
      } else {
         return absurd()
      }
   }

   cont (κ: Cont): [SVGElement[], SVGElement][] {
      if (κ instanceof Expr.Expr) {
         const g: SVGElement = 
            κ instanceof Expr.Fun ?
            this.elim(κ.σ) :
            this.horizSpace(this.keyword("arrow"), this.expr(κ))
         return [[[], g]]
      } else
      if (κ instanceof Elim) {
         return this.clauses(κ)
      } else {
         return absurd()
      }
   }
   
   def (def: Expr.Def): SVGElement {
      if (def instanceof Expr.Prim) {
         return this.horizSpace(this.keyword("primitive"), this.text(def.x.val))
      } else
      if (def instanceof Expr.Let) {
         if (def.e instanceof Expr.Fun) {
            return this.horizSpace(this.keyword("let_"), this.text(def.x.val), this.elim(def.e.σ))
         } else {
            return this.horizSpace(this.keyword("let_"), this.keyword("equals"), this.expr(def.e))
         }
      } else
      if (def instanceof Expr.LetRec) {
         return this.horizSpace(this.keyword("letRec"), this.vert(...def.δ.toArray().map(def => this.recDef(def))))
      } else {
         return absurd()
      }
   }

   elim<K extends Cont> (σ: Elim<K>): SVGElement {
      return this.vert(...this.clauses(σ).map(([gs, g]) => this.horizSpace(...gs, g)))
   }

   // Post-condition: returned element has an entry in "dimensions" map. 
   expr (e: Expr): SVGElement {
      if (e instanceof Expr.ConstNum) {
         return this.num_(e.val, true)
      } else
      if (e instanceof Expr.ConstStr) {
         return this.text(e.val.toString())
      } else
      if (e instanceof Expr.DataExpr) {
         if (hasExprClass(e, Pair)) {
            return this.pair(e, as(e.__child("fst"), Expr.Expr), as(e.__child("snd"), Expr.Expr))
         } else
         if (hasExprClass(e, Nil) || hasExprClass(e, Cons)) {
            const g: SVGElement = this.list(exprElements(e))
            // TEMPORARY EXPERIMENT
            as(g.childNodes[0], SVGElement).addEventListener("click", (ev: MouseEvent): void => {
               ev.stopPropagation()
               __deltas.clear()
               new ExprCursor(e).constr_splice(Cons, ["head"], ([e]: Expr[]): [Expr] => {
                  const eʹ: Expr = Expr.app(Expr.var_(str("sq")(ν()))(ν()), Expr.var_(str("x")(ν()))(ν()))(ν())
                  return [at(exprClass(Pair.name), e, eʹ)(ν())]
               })
               this.editor.onEdit()
            })
            // END TEMPORARY EXPERIMENT
            return g
         } else {
            return this.unimplemented(e)
         }
      } else
      if (e instanceof Expr.Var) {
         return this.text(e.x.val)
      } else
      if (e instanceof Expr.Fun) {
         return this.horizSpace(this.keyword("fun"), this.keyword("arrow"), this.elim(e.σ))
      } else
      if (e instanceof Expr.BinaryApp) {
         return this.horizSpace(this.expr(e.e1), this.text(e.opName.val), this.expr(e.e2))
      } else
      if (e instanceof Expr.App) {
         return this.horizSpace(
            e.f instanceof Expr.Fun ? this.parenthesise(this.expr(e.f)) : this.expr(e.f), 
            e.e instanceof Expr.Fun ? this.parenthesise(this.expr(e.e)) : this.expr(e.e)
         )
      } else
      if (e instanceof Expr.Defs) {
         return this.vert(
            this.vert(...e.def̅.toArray().map(def => this.def(def))),
            this.expr(e.e)
         )
      } else {
         return absurd()
      }
   }

   exprOrValue (v: Value): SVGElement {
      if (v instanceof Expr.Expr) {
         return this.expr(v)
      } else {
         return this.value(v)
      }
   }

   horiz (...gs: SVGElement[]): SVGElement {
      const g: SVGGElement = document.createElementNS(SVG.NS, "svg")
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

   horizSpace (...gs: SVGElement[]): SVGElement {
      const gsʹ: SVGElement[] = []
      gs.forEach((g: SVGElement, n: number): void => {
         gsʹ.push(g)
         if (n < gs.length - 1) {
            gsʹ.push(this.space())
         }
      })
      return this.horiz(...gsʹ)
   }

   keyword (str: keyof typeof strings, ẟ_style?: string): SVGElement {
      return this.text(strings[str], ẟ_style)
   }

   list ([es, eʹ]: [Value[], Value | null]): SVGElement {
      const gs: SVGElement[] = []
      es.forEach((e: Value, n: number): void => {
         gs.push(this.exprOrValue(e))
         if (n < es.length - 1) {
            gs.push(this.keyword("comma"), this.space())
         }
      })
      if (eʹ !== null) {
         gs.push(this.keyword("comma"), this.space(), this.keyword("ellipsis"), this.exprOrValue(eʹ))
      }
      return this.horiz(this.keyword("bracketL"), ...gs, this.keyword("bracketR"))
   }

   num_ (n: Num, editable: boolean): SVGElement {
      const g: SVGElement = this.text(n.toString(), deltaStyle(n))
      if (editable && Number.isInteger(n.val)) {
         g.addEventListener("click", (ev: MouseEvent): void => {
            __deltas.clear()
            new ExprCursor(n).setNum(n.val + 1)
            ev.stopPropagation()
            this.editor.onEdit()
         })
      }
      return g
   }

   pair (e: Value, e1: Value, e2: Value): SVGElement {
      return this.horiz(
         this.keyword("parenL", deltaStyle(e)), 
         this.exprOrValue(e1),
         this.keyword("comma", deltaStyle(e)),
         this.space(), 
         this.exprOrValue(e2),
         this.keyword("parenR", deltaStyle(e))
      )
   }

   parenthesise (g: SVGElement): SVGElement {
      return this.horiz(this.keyword("parenL"), g, this.keyword("parenR"))
   }

   prompt (e: Expr, v: Value): SVGElement {
      const g: SVGElement = this.vert(
         this.expr(e),
         this.horizSpace(this.text(">"), this.value(v))
      )
      g.setAttribute("x", `0`)
      g.setAttribute("y", `0`)
      return g
   }

   recDef (def: Expr.RecDef): SVGElement {
      return this.horizSpace(this.text(def.x.val), this.elim(def.σ))
   }

   space (): SVGElement {
      return this.text(`${space}`)
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

   unimplemented (v: Value): SVGElement {
      return this.text(`<${className(v)}>`)
   }

   value (v: Value): SVGElement {
      if (v instanceof Num) {
         return this.num_(v, false)
      } else
      if (v instanceof Str) {
         return this.text(v.val.toString(), deltaStyle(v))
      } else 
      if (v instanceof List) {
         return this.list([v.toArray(), null])
      } else
      if (v instanceof Pair) {
         return this.pair(v, v.fst, v.snd)
      } else {
         return this.unimplemented(v)
      }
   }

   vert (...gs: SVGElement[]): SVGElement {
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

// Expressions for (initial) elements of a list, plus expression for tail (or null if list terminates with nil).
function exprElements (e: Expr): [Expr[], Expr | null] {
   if (e instanceof Expr.DataExpr) {
      if (hasExprClass(e, Nil)) {
         return [[], null]
      } else
      if (hasExprClass(e, Cons)) {
         // use cursor interface instead?
         const [es, eʹ]: [Expr[], Expr | null] = exprElements(as(e.__child("tail"), Expr.Expr))
         return [[as(e.__child("head"), Expr.Expr), ...es], eʹ]
      } else {
         return error(`Found ${e.ctr}, expected list.`)
      }
   } else {
      return [[], e]
   }
}
