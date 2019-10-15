import { flatten, nth, zip } from "../util/Array"
import { Class, __nonNull, absurd, as, assert, className, error } from "../util/Core"
import { Cons, List, Nil, Pair } from "../BaseTypes"
import { Ctr, ctrFor, exprClass } from "../DataType"
import { DataValue } from "../DataValue"
import { Change, New, Reclassify } from "../Delta"
import { Eval } from "../Eval"
import { Expr, strings } from "../Expr"
import { DataElim, Elim, VarElim } from "../Match"
import { Num, Str, Value, fields, isPrim } from "../Value"
import { ν, at, newRevision, str, versioned } from "../Versioned"
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

function isExprFor (e: Expr, C: Class<DataValue>): boolean {
   return className(e) === exprClass(C).name
}

// To visualise an eliminator, we reconstruct the patterns from the trie. List syntax in particular doesn't have
// an analogous "case tree" form.
type PatternElement = [Ctr | Str, DeltaStyle]

export interface EditListener {
   onEdit (): void
}

function compareCtr (c1: string, c2: string): number {
   const n: number = ctrFor(c1).arity - ctrFor(c2).arity
   return n === 0 ? c1.localeCompare(c2) : n
}

export class Renderer {
   editor: EditListener

   constructor (editor: EditListener) {
      this.editor = editor
   }

   arrow (): SVGElement {
      return this.keyword("arrow")
   }

   bracket (...gs: SVGElement[]): SVGElement {
      return this.horiz(this.keyword("bracketL"), ...gs, this.keyword("bracketR"))
   }

   comma (ẟ_style?: DeltaStyle): SVGElement {
      return this.keyword("comma", ẟ_style)
   }

   commaDelimit (...gs: SVGElement[]): SVGElement[] {
      return this.delimit(() => this.horiz(this.comma(), this.space()), ...gs)
   }

   // Generic over whether we have a data value or a data expression.
   dataConstr (parens: boolean, e: DataValue | Expr.DataExpr): SVGElement {
      const es: Value[] = e.__children
      const g: SVGElement = this.horizSpace(this.text(e.ctr, deltaStyle(e)), ...es.map(eʹ => this.exprOrValue(true, eʹ)))
      return this.parenthesiseIf(es.length > 0 && parens, g, deltaStyle(e))
   }

   def (def: Expr.Def): SVGElement {
      if (def instanceof Expr.Prim) {
         return this.horizSpace(this.keyword("primitive", deltaStyle(def)), this.patternVar(def.x))
      } else
      if (def instanceof Expr.Let) {
         if (def.e instanceof Expr.Fun) {
            return this.horizSpace(this.keyword("let_", deltaStyle(def)), this.patternVar(def.x), this.elim(def.e.σ))
         } else {
            return this.horizSpace(this.keyword("let_", deltaStyle(def)), this.patternVar(def.x), this.keyword("equals", deltaStyle(def)), this.expr(false, def.e))
         }
      } else
      if (def instanceof Expr.LetRec) {
         return this.horizSpace(this.keyword("letRec", deltaStyle(def)), this.vert(...def.δ.toArray().map(def => this.recDef(def))))
      } else {
         return absurd()
      }
   }

   delimit (delimiter: () => SVGElement, ...gs: SVGElement[]): SVGElement[] {
      const gsʹ: SVGElement[] = []
      gs.forEach((g: SVGElement, n: number): void => {
         gsʹ.push(g)
         if (n < gs.length - 1) {
            gsʹ.push(delimiter())
         }
      })
      return gsʹ
   }

   elim<K extends Cont> (σ: Elim<K>): SVGElement {
      return this.vert(...clauses(σ).map(([cxs, e]) => {
         const [[g], cxsʹ]: [SVGElement[], PatternElement[]] = this.patterns(false, 1, cxs)
         assert(cxsʹ.length === 0)
         const gʹ: SVGElement = 
            e instanceof Expr.Fun ?
            this.elim(e.σ) : // curried function resugaring
            this.horizSpace(this.arrow(), this.expr(false, e))
         return this.horizSpace(g, gʹ)
      }))
   }

   ellipsis (ẟ_style?: DeltaStyle): SVGElement {
      return this.keyword("ellipsis", ẟ_style)
   }

   // Post-condition: returned element has an entry in "dimensions" map. 
   expr (parens: boolean, e: Expr): SVGElement {
      if (e instanceof Expr.ConstNum) {
         return this.num(e.val, true)
      } else
      if (e instanceof Expr.ConstStr) {
         return this.text(e.val.toString())
      } else
      if (e instanceof Expr.Fun) {
         const g: SVGElement = this.horizSpace(this.keyword("fun", deltaStyle(e)), this.elim(e.σ))
         return this.parenthesiseIf(parens, g, deltaStyle(e))
      } else
      if (e instanceof Expr.DataExpr) {
         if (isExprFor(e, Pair)) {
            return this.pair(e, as(e.__child("fst"), Expr.Expr), as(e.__child("snd"), Expr.Expr))
         } else
         if (isExprFor(e, Nil) || isExprFor(e, Cons)) {
            const g: SVGElement = this.list(exprElements(e))
            // TEMPORARY EXPERIMENT
            as(g.childNodes[0], SVGElement).addEventListener("click", (ev: MouseEvent): void => {
               ev.stopPropagation()
               newRevision()
               new ExprCursor(e).constr_splice(Cons, ["head"], ([e]: Expr[]): [Expr] => {
                  const eʹ: Expr = Expr.app(Expr.var_(str("sq")(ν()))(ν()), Expr.var_(str("x")(ν()))(ν()))(ν())
                  return [at(exprClass(Pair), e, eʹ)(ν())]
               })
               this.editor.onEdit()
            })
            // END TEMPORARY EXPERIMENT
            return g
         } else {
            return this.dataConstr(parens, e)
         }
      } else
      if (e instanceof Expr.Quote) {
         return this.unimplemented(e)
      } else
      if (e instanceof Expr.Var) {
         return this.text(e.x.val, deltaStyle(e.x))
      } else
      if (e instanceof Expr.App) {
         return this.parenthesiseIf(
            parens, 
            this.horizSpace(this.expr(!(e.f instanceof Expr.App), e.f), this.expr(true, e.e)),
            deltaStyle(e)
         )
      } else
      if (e instanceof Expr.BinaryApp) {
         // ignore operator precedence, but allow function application to take priority over any binary operation
         return this.parenthesiseIf(
            parens, 
            this.horizSpace(
               this.expr(!(e.e1 instanceof Expr.App), e.e1), 
               this.text(e.opName.val, deltaStyle(e.opName)), 
               this.expr(!(e.e2 instanceof Expr.App), e.e2)
            ),
            deltaStyle(e)
         )
      } else
      if (e instanceof Expr.Defs) {
         return this.parenthesiseIf(parens,
            this.vert(
               this.vert(...e.def̅.toArray().map(def => this.def(def))),
               this.expr(false, e.e)
            )
         )
      } else
      if (e instanceof Expr.MatchAs) {
         return this.vert(
            this.horizSpace(this.keyword("match", deltaStyle(e)), this.expr(false, e.e), this.keyword("as", deltaStyle(e))),
            this.elim(e.σ)
         )
      } else
      if (e instanceof Expr.Typematch) {
         return this.vert(
            this.horizSpace(this.keyword("typematch", deltaStyle(e)), this.expr(false, e.e), this.keyword("as", deltaStyle(e))),
            ...e.cases.toArray().map(({fst: x, snd: e}: Pair<Str, Expr>) => 
               this.horizSpace(this.text(x.val), this.arrow(), this.expr(false, e))
            )
         )
      } else {
         return absurd(`Unimplemented expression form: ${className(e)}.`)
      }
   }

   exprOrValue (parens: boolean, v: Value): SVGElement {
      if (v instanceof Expr.Expr) {
         return this.expr(parens, v)
      } else {
         return this.value(parens, v)
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
      return this.horiz(...this.delimit(() => this.space(), ...gs))
   }

   keyword (str: keyof typeof strings, ẟ_style?: DeltaStyle): SVGElement {
      return this.text(strings[str], ẟ_style)
   }

   // Generic over whether we have a list or a list expression.
   list ([es, eʹ]: [Value[], Value | null]): SVGElement {
      return this.bracket(
         ...this.commaDelimit(...es.map(e => this.exprOrValue(false, e))),
         ...(eʹ === null ? [] : [this.comma(), this.space(), this.ellipsis(), this.exprOrValue(false, eʹ)])
      )
   }

   listPattern (cx: PatternElement, cxs: PatternElement[]): [SVGElement, PatternElement[]] {
      const gs: SVGElement[] = []
      while (cx[0] instanceof Ctr && cx[0].C === Cons) {
         const [[g], cxsʹ]: [SVGElement[], PatternElement[]] = this.patterns(false, 1, cxs)
         gs.push(g)
         cx = nth(cxsʹ, 0) // tail must be another Cons/Nil pattern element, or a variable
         cxs = cxsʹ.splice(1)
      }
      const gsʹ: SVGElement[] =
         cx[0] instanceof Str ?
            [this.comma(), this.space(), this.ellipsis(), this.patternVar(cx[0])] :
            cx[0].C === Nil ? 
               [] : 
               absurd()
      return [this.bracket(...this.commaDelimit(...gs), ...gsʹ), cxs]
   }

   num (n: Num, editable: boolean): SVGElement {
      const g: SVGElement = this.text(n.toString(), deltaStyle(n))
      if (editable && Number.isInteger(n.val)) {
         g.addEventListener("click", (ev: MouseEvent): void => {
            newRevision()
            new ExprCursor(n).setNum(n.val + 1)
            ev.stopPropagation()
            this.editor.onEdit()
         })
      }
      return g
   }

   pair (e: Value, e1: Value, e2: Value): SVGElement {
      return this.parenthesise(
         this.horiz(
            this.exprOrValue(false, e1),
            this.comma(deltaStyle(e)),
            this.space(), 
            this.exprOrValue(false, e2)
         ), 
         deltaStyle(e)
      )
   }

   parenthesise (g: SVGElement, ẟ_style?: DeltaStyle): SVGElement {
      return this.horiz(this.keyword("parenL", ẟ_style), g, this.keyword("parenR", ẟ_style))
   }

   parenthesiseIf (parens: boolean, g: SVGElement, ẟ_style?: DeltaStyle): SVGElement {
      return parens ? this.parenthesise(g, ẟ_style) : g
   }

   patterns (parens: boolean, n: number, cxs: PatternElement[]): [SVGElement[], PatternElement[]] {
      if (n === 0) {
         return [[], cxs]
      } else
      if (cxs[0][0] instanceof Ctr) {
         const ctr: Ctr = cxs[0][0]
         if (ctr.C === Pair) {
            const [[g1, g2], cxsʹ]: [SVGElement[], PatternElement[]] = this.patterns(false, 2, cxs.slice(1))
            const [gsʹ, cxsʹʹ]: [SVGElement[], PatternElement[]] = this.patterns(parens, n - 1, cxsʹ)
            return [[this.parenthesise(this.horiz(g1, this.comma(), this.space(), g2)), ...gsʹ], cxsʹʹ]
         } else
         if (ctr.C === Nil || ctr.C === Cons) {
            const [g, cxsʹ]: [SVGElement, PatternElement[]] = this.listPattern(cxs[0], cxs.slice(1))
            const [gs, cxsʹʹ]: [SVGElement[], PatternElement[]] = this.patterns(parens, n - 1, cxsʹ)
            return [[g, ...gs], cxsʹʹ]
         } else {
            const [gs, cxsʹ]: [SVGElement[], PatternElement[]] = this.patterns(true, ctr.arity, cxs.slice(1))
            const g: SVGElement = this.horizSpace(this.text(ctr.c), ...gs)
            const [gsʹ, cxsʹʹ]: [SVGElement[], PatternElement[]] = this.patterns(parens, n - 1, cxsʹ)
            return [[this.parenthesiseIf(ctr.arity > 0 && parens, g), ...gsʹ], cxsʹʹ]
         }
      } else
      if (cxs[0][0] instanceof Str) {
         const x: Str = cxs[0][0]
         const [gs, cxsʹ]: [SVGElement[], PatternElement[]] = this.patterns(parens, n - 1, cxs.slice(1))
         return [[this.patternVar(x), ...gs], cxsʹ]
      } else {
         return absurd()
      }
   }

   patternVar (x: Str): SVGElement {
      return this.text(x.val, deltaStyle(x))
   }

   prompt (e: Expr, v: Value): SVGElement {
      const g: SVGElement = this.vert(
         this.expr(false, e),
         this.horizSpace(this.text(">"), this.value(false, v))
      )
      g.setAttribute("x", `0`)
      g.setAttribute("y", `0`)
      return g
   }

   recDef (def: Expr.RecDef): SVGElement {
      return this.horizSpace(this.patternVar(def.x), this.elim(def.σ))
   }

   space (): SVGElement {
      return this.text(`${space}`)
   }

   text (str: string, ẟ_style?: DeltaStyle): SVGTextElement {
      ẟ_style = ẟ_style || DeltaStyle.Unchanged // default
      const text: SVGTextElement = textElement(0, 0, fontSize, [classes, ẟ_style].join(" "), str)
      text.setAttribute("transform", `translate(${0},${lineHeight})`)
      const width: number = svg.textWidth(text)
      dimensions.set(text, { width, height: lineHeight })
      text.remove()
      return text
   }

   unimplemented (v: Value): SVGElement {
      throw new Error(`TODO: ${className(v)}`)
   }

   value (parens: boolean, v: Value): SVGElement {
      if (v instanceof Num) {
         return this.num(v, false)
      } else
      if (v instanceof Str) {
         return this.text(v.toString(), deltaStyle(v))
      } else
      if (v instanceof Eval.Closure) {
         return this.unimplemented(v)
      } else
      if (v instanceof DataValue) {
         if (v instanceof List) {
            return this.list([v.toArray(), null])
         } else
         if (v instanceof Pair) {
            return this.pair(v, v.fst, v.snd)
         } else {
            return this.dataConstr(parens, v)
         }
      } else {
         return absurd()
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

function cont (κ: Cont): [PatternElement[], Expr][] {
   if (κ instanceof Expr.Expr) {
      return [[[], κ]]
   } else
   if (κ instanceof Elim) {
      return clauses(κ)
   } else {
      return absurd()
   }
}

function clauses<K extends Cont> (σ: Elim<K>): [PatternElement[], Expr][] {
   if (VarElim.is(σ)) {
      const cs: [PatternElement[], Expr][] = cont(σ.κ)
      // disregard any delta information on x :-/
      return cs.map(([cxs, e]) => [[[σ.x, deltaStyle(σ)], ...cxs], e])
   } else
   if (DataElim.is(σ)) {
      const cκs: [string, Cont][] = zip(fields(σ), σ.__children as Cont[]).sort(([c1, ], [c2, ]): number => compareCtr(c1, c2))
      return flatten(cκs.filter(([c, κ]) => κ !== undefined).map(([c, κ]): [PatternElement[], Expr][] =>
         cont(__nonNull(κ)).map(([cxs, e]: [PatternElement[], Expr]) => [[[ctrFor(c), deltaStyle(σ)], ...cxs], e])
      ))
   } else {
      return absurd()
   }
}

enum DeltaStyle {
   New = "new",
   Changed = "changed",
   Unchanged = "unchanged"
}

// Delta-styling for the constructor component of a value (not its child pointers). In particular, primitives appear changed
// iff their value has changed, whereas non-primitives appear changed iff reclassified. Changes to child pointers must be
// visualised separately.
function deltaStyle (v: Value): DeltaStyle {
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

// Expressions for (initial) elements of a list, plus expression for tail (or null if list terminates with nil).
function exprElements (e: Expr): [Expr[], Expr | null] {
   if (e instanceof Expr.DataExpr) {
      if (isExprFor(e, Nil)) {
         return [[], null]
      } else
      if (isExprFor(e, Cons)) {
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
