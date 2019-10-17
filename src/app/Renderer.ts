import { flatten, nth, zip } from "../util/Array"
import { Class, __nonNull, absurd, as, assert, className, classOf, error } from "../util/Core"
import { Cons, List, Nil, Pair } from "../BaseTypes"
import { Ctr, ctrFor, explClass, exprClass } from "../DataType"
import { DataValue, ExplValue, explValue } from "../DataValue"
import { Change, New, Reclassify } from "../Delta"
import { Eval } from "../Eval"
import { Expl } from "../Expl"
import { Expr, strings } from "../Expr"
import { DataElim, Elim, Match, VarElim } from "../Match"
import { ApplicationId, Num, Str, TaggedId, Value, fields, isPrim } from "../Value"
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

function isExplFor (t: Expl, C: Class<DataValue>): boolean {
   return classOf(t) === explClass(C)
}

function isExprFor (e: Expr, C: Class<DataValue>): boolean {
   return classOf(e) === exprClass(C)
}

// Unpack evaluation memo-key to recover original expression.
function exprFor (t: Expl): Expr {
   if (versioned(t)) {
      return as(as(as(t.__id, TaggedId).k, ApplicationId).v, Expr.Expr)
   } else {
      return absurd()
   }
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

   arrow (ẟ_style: DeltaStyle): SVGElement {
      return this.keyword("arrow", ẟ_style)
   }

   bracket (gs: SVGElement[], ẟ_style: DeltaStyle): SVGElement {
      return this.horiz(this.keyword("bracketL", ẟ_style), ...gs, this.keyword("bracketR", ẟ_style))
   }

   comma (ẟ_style: DeltaStyle): SVGElement {
      return this.keyword("comma", ẟ_style)
   }

   // Generic over whether we have a data value or data expression.
   dataConstr (parens: boolean, e: DataValue): SVGElement {
      const es: Value[] = e.__children
      const g: SVGElement = this.horizSpace(this.text(e.ctr, deltaStyle(e)), ...es.map(eʹ => this.exprOrExplValueOrValue(true, eʹ)))
      return this.parenthesiseIf(es.length > 0 && parens, g, deltaStyle(e))
   }

   dataConstrExpl (parens: boolean, {t, v}: ExplValue<DataValue>): SVGElement {
      const tvs: ExplValue[] = Expl.explChildren(t, v)
      // a constructor expression makes its value, so their root delta highlighting must agree
      const g: SVGElement = this.horizSpace(this.text(v.ctr, deltaStyle(v)), ...tvs.map(tvʹ => this.explValue(true, tvʹ)))
      return this.parenthesiseIf(tvs.length > 0 && parens, g, deltaStyle(v))
   }

   def (def: Expr.Def): SVGElement {
      if (def instanceof Expr.Prim) {
         return this.horizSpace(this.keyword("primitive", deltaStyle(def)), this.patternVar(def.x))
      } else
      if (def instanceof Expr.Let) {
         if (def.e instanceof Expr.Fun) {
            return this.horizSpace(this.keyword("let_", deltaStyle(def)), this.patternVar(def.x), this.elim(def.e.σ))
         } else {
            return this.horizSpace(
               this.keyword("let_", deltaStyle(def)), 
               this.patternVar(def.x), 
               this.keyword("equals", deltaStyle(def)), 
               this.expr(false, def.e)
            )
         }
      } else
      if (def instanceof Expr.LetRec) {
         return this.horizSpace(this.keyword("letRec", deltaStyle(def)), this.vert(...def.δ.toArray().map(def => this.recDef(def))))
      } else {
         return absurd()
      }
   }

   defₜ (def: Expl.Def): SVGElement {
      if (def instanceof Expl.Prim) {
         return this.horizSpace(this.keyword("primitive", deltaStyle(def)), this.patternVar(def.x))
      } else
      if (def instanceof Expl.Let) {
         if (def.tv.t instanceof Expl.Const && def.tv.v instanceof Eval.Closure) {
            return this.horizSpace(this.keyword("let_", deltaStyle(def)), this.patternVar(def.x), this.elim(def.tv.v.f))
         } else {
            return this.horizSpace(
               this.keyword("let_", deltaStyle(def)), 
               this.patternVar(def.x), 
               this.keyword("equals", deltaStyle(def)), 
               this.explValue(false, def.tv)
            )
         }
      } else
      if (def instanceof Expl.LetRec) {
         return this.horizSpace(this.keyword("letRec", deltaStyle(def)), this.vert(...def.δ.toArray().map(def => this.recDefₜ(def))))
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
            this.horizSpace(this.arrow(deltaStyle(e)), this.expr(false, e))
         return this.horizSpace(g, gʹ)
      }))
   }

   elimMatch<K extends Cont> (ξ: Match<K>): SVGElement {
      return this.unimplemented(ξ)
   }

   ellipsis (ẟ_style: DeltaStyle): SVGElement {
      return this.keyword("ellipsis", ẟ_style)
   }

   explValue (parens: boolean, tv: ExplValue): SVGElement {
      const [gs, g] = this.explValue_aux(parens, tv)
      let gʹ: SVGElement
      if (gs.length === 0) {
         if (g !== null) {
            gʹ = g
         } else {
            return error("Must visualise either trace or value.")
         }
      } else {
         if (g === null) {
            gʹ = this.vert(...gs)
         } else {
            gʹ = this.horizSpace(this.vert(...gs), this.text("▸", DeltaStyle.Unchanged), g)
         }
      }
      if (gʹ instanceof SVGSVGElement) {
         return this.selectionHighlight(gʹ)
      } else {
         return gʹ
      }
   }

   selectionHighlight (g: SVGSVGElement): SVGElement {
      const border: SVGRectElement = document.createElementNS(SVG.NS, "rect")
      border.setAttribute("x", g.x.baseVal.valueAsString) // is there an easier way?
      border.setAttribute("y", g.y.baseVal.valueAsString)
      const { width, height }: Dimensions = dimensions.get(g)!
      border.setAttribute("height", height.toString())
      border.setAttribute("width", width.toString())
      border.setAttribute("stroke", "gray")
      border.setAttribute("fill", "none")
      g.appendChild(border)
      return g
   }

   // Returns a list of trace views and a value view.
   explValue_aux (parens: boolean, {t, v}: ExplValue): [SVGElement[], SVGElement | null] {
      if (t instanceof Expl.Const) {
         if (v instanceof Num) {
            return [[], this.num(v, as(exprFor(t), Expr.ConstNum).val)]
         } else
         if (v instanceof Str) {
            return [[], this.str(v)]
         } else {
            return [[], this.unimplemented(v)]
         }
      } else
      if (t instanceof Expl.DataExpl) {
         if (isExplFor(t, Pair)) {
            const vʹ: Pair = v as Pair
            return [[], this.pair(t, Expl.explChild(t, vʹ, "fst"), Expl.explChild(t, vʹ, "snd"))]
         } else
         if (isExplFor(t, Nil) || isExplFor(t, Cons)) {
            return [[], this.listExpl(explValue(t, v as List))]
         } else {
            return [[], this.dataConstrExpl(parens, explValue(t, v as DataValue))]
         }
      } else
      if (t instanceof Expl.Var) {
         // ouch: disregard delta-info on trace itself
         // values of variables themselves have explanations, but ignore those for now
         const g_opt: SVGElement | null = 
            v instanceof Eval.Closure ? null : this.explValue_aux(parens, explValue(t.t, v))[1] 
         return [[this.text(t.x.val, deltaStyle(t))], g_opt]
      } else
      if (t instanceof Expl.UnaryApp) {
         const g: SVGElement = this.parenthesiseIf(
            parens, 
            this.explValue(!(t.tf.t instanceof Expl.App), t.tf),
            deltaStyle(t)
         )
         return [[g], this.value(parens, v)]
      } else
      if (t instanceof Expl.BinaryApp) {
         const g: SVGElement = this.parenthesiseIf(
            parens, 
            this.horizSpace(
               this.explValue(!(t.tv1.t instanceof Expl.App), t.tv1), 
               this.text(t.opName.val, deltaStyle(t)), // what about changes associated with t.opName? 
               this.explValue(!(t.tv2.t instanceof Expl.App), t.tv2)
            ),
            deltaStyle(t)
         )
         return [[g], this.value(parens, v)]
      } else
      if (t instanceof Expl.App) {
         const [gs, g] = this.explValue_aux(parens, explValue(t.t, v))
         const gʹ: SVGElement = this.parenthesiseIf(
            parens, 
            this.horizSpace(this.explValue(!(t.tf.t instanceof Expl.App), t.tf), this.explValue(true, t.tu)),
            deltaStyle(t)
         )
         return [[gʹ, ...gs], g]
      } else 
      if (t instanceof Expl.Defs) {
         const [gs, g] = this.explValue_aux(parens, explValue(t.t, v))
         const gʹ: SVGElement = this.parenthesiseIf(
            parens,
            this.vert(...t.def̅.toArray().map(def => this.defₜ(def))),
            deltaStyle(t)
         )
         return [[gʹ, ...gs], g]
      } else
      if (t instanceof Expl.MatchAs) {
         const [gs, g] = this.explValue_aux(parens, explValue(t.t, v))
         const gʹ: SVGElement = this.vert(
            this.horizSpace(this.keyword("match", deltaStyle(t)), this.explValue(false, t.tu), this.keyword("as", deltaStyle(t))),
            this.elimMatch(t.ξ)
         )
         return [[gʹ, ...gs], g]
      } else {
         return [[this.unimplemented(t)], this.unimplemented(v)]
      }
   }

   // Post-condition: returned element has an entry in "dimensions" map. 
   expr (parens: boolean, e: Expr): SVGElement {
      if (e instanceof Expr.ConstNum) {
         // ouch: disregard delta-info on expression itself
         return this.num(e.val, e.val)
      } else
      if (e instanceof Expr.ConstStr) {
         // ouch: disregard delta-info on expression itself
         return this.str(e.val)
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
            const g: SVGElement = this.listExpr(e)
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
         // ouch: disregard delta-info on Var.x
         return this.text(e.x.val, deltaStyle(e))
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
               this.text(e.opName.val, deltaStyle(e)), // what about changes associated with e.opName 
               this.expr(!(e.e2 instanceof Expr.App), e.e2)
            ),
            deltaStyle(e)
         )
      } else
      if (e instanceof Expr.Defs) {
         return this.parenthesiseIf(
            parens,
            this.vert(
               this.vert(...e.def̅.toArray().map(def => this.def(def))),
               this.expr(false, e.e)
            ),
            deltaStyle(e)
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
               this.horizSpace(this.text(x.val, deltaStyle(x)), this.arrow(deltaStyle(e)), this.expr(false, e))
            )
         )
      } else {
         return absurd(`Unimplemented expression form: ${className(e)}.`)
      }
   }

   exprOrExplValueOrValue (parens: boolean, v: Value): SVGElement {
      if (v instanceof Expr.Expr) {
         return this.expr(parens, v)
      } else
      if (v instanceof ExplValue) {
         return this.explValue(parens, v)
      } else {
         return this.value(parens, v)
      }
   }

   horiz (...gs: SVGElement[]): SVGSVGElement {
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

   horizSpace (...gs: SVGElement[]): SVGSVGElement {
      return this.horiz(...this.delimit(() => this.space(), ...gs))
   }

   keyword (str: keyof typeof strings, ẟ_style: DeltaStyle): SVGElement {
      return this.text(strings[str], ẟ_style)
   }

   list (vs: List<Value>): SVGElement {
      const gs: SVGElement[] = []
      for (; Cons.is(vs); vs = vs.tail) {
         gs.push(this.value(false, vs.head))
         // associate every Cons, apart from the last one, with a comma
         if (Cons.is(vs.tail)) {
            gs.push(this.comma(deltaStyle(vs)), this.space())
         }
      }
      // brackets correspond to the nil
      return this.bracket(gs, deltaStyle(vs))
   }

   // Difficult to make this generic enough to use for list values too.
   listExpr (e: Expr): SVGElement {
      const gs: SVGElement[] = []
      while (isExprFor(e, Cons)) {
         gs.push(this.exprOrExplValueOrValue(false, e.__child("head") as Expr | Expl))
         const eʹ: Expr = e.__child("tail") as Expr
         if (!(isExprFor(eʹ, Nil))) {
            // associate every Cons, apart from the last one, with a comma
            gs.push(this.comma(deltaStyle(e)), this.space())
         }
         e = eʹ
      }
      if (isExprFor(e, Nil)) {
         return this.bracket(gs, deltaStyle(e))
      } else {
         // non-list expression in tail position determines delta-highlighting for brackets and ellipsis as well
         return this.bracket(
            [...gs, this.space(), this.ellipsis(deltaStyle(e)), this.exprOrExplValueOrValue(false, e)], 
            deltaStyle(e)
         )
      }
   }

   // Ouch, highly redundant with listExpr
   listExpl ({t, v}: ExplValue<List>): SVGElement {
      const gs: SVGElement[] = []
      while (isExplFor(t, Cons)) {
         const vʹ: Cons = v as Cons
         gs.push(this.exprOrExplValueOrValue(false, Expl.explChild(t, vʹ, "head")))
         const {t: tʹ, v: vʹʹ}: ExplValue = Expl.explChild(t, vʹ, "tail")
         if (!(isExplFor(tʹ, Nil))) {
            // associate every Cons, apart from the last one, with a comma
            gs.push(this.comma(deltaStyle(t)), this.space())
         }
         t = tʹ
         v = as(vʹʹ, List)
      }
      if (isExplFor(t, Nil)) {
         return this.bracket(gs, deltaStyle(t))
      } else {
         // non-list expression in tail position determines delta-highlighting for brackets and ellipsis as well
         return this.bracket(
            [...gs, this.space(), this.ellipsis(deltaStyle(t)), this.exprOrExplValueOrValue(false, explValue(t, v))], 
            deltaStyle(t)
         )
      }
   }

   listPattern ([ctr_x, ẟ_style]: PatternElement, cxs: PatternElement[]): [SVGElement, PatternElement[]] {
      const gs: SVGElement[] = []
      while (ctr_x instanceof Ctr && ctr_x.C === Cons) {
         const [[g], cxsʹ]: [SVGElement[], PatternElement[]] = this.patterns(false, 1, cxs)
         gs.push(g)
         let ẟ_styleʹ: DeltaStyle
         ;[ctr_x, ẟ_styleʹ] = nth(cxsʹ, 0) // tail must be another Cons/Nil pattern element, or a variable
         // associate every Cons, apart from the last one, with a comma
         if (!(ctr_x instanceof Ctr && ctr_x.C === Nil)) {
            gs.push(this.comma(ẟ_style), this.space())
         }
         cxs = cxsʹ.splice(1)
         ẟ_style = ẟ_styleʹ
      }
      if (ctr_x instanceof Str) {
         // pattern variable in tail position determines delta-highlighting for brackets and ellipsis as well
         return [this.bracket([...gs, this.ellipsis(deltaStyle(ctr_x)), this.patternVar(ctr_x)], deltaStyle(ctr_x)), cxs]
      } else
      if (ctr_x.C === Nil) {
         // otherwise brackets correspond to the nil
         return [this.bracket(gs, ẟ_style), cxs]
      } else {
         return absurd()
      }
   }

   num (n: Num, src?: Num): SVGElement {
      const g: SVGElement = this.text(n.toString(), deltaStyle(n))
      if (src && Number.isInteger(src.val)) {
         g.addEventListener("click", (ev: MouseEvent): void => {
            newRevision()
            new ExprCursor(src).setNum(src.val + 1)
            ev.stopPropagation()
            this.editor.onEdit()
         })
      }
      return g
   }

   pair (e: Value, e1: Value, e2: Value): SVGElement {
      return this.parenthesise(
         this.horiz(
            this.exprOrExplValueOrValue(false, e1),
            this.comma(deltaStyle(e)),
            this.space(), 
            this.exprOrExplValueOrValue(false, e2)
         ), 
         deltaStyle(e)
      )
   }

   parenthesise (g: SVGElement, ẟ_style: DeltaStyle): SVGElement {
      return this.horiz(this.keyword("parenL", ẟ_style), g, this.keyword("parenR", ẟ_style))
   }

   parenthesiseIf (parens: boolean, g: SVGElement, ẟ_style: DeltaStyle): SVGElement {
      return parens ? this.parenthesise(g, ẟ_style) : g
   }

   patterns (parens: boolean, n: number, cxs: PatternElement[]): [SVGElement[], PatternElement[]] {
      if (n === 0) {
         return [[], cxs]
      } else {
         const [ctr_x, ẟ_style] = cxs[0]
         if (ctr_x instanceof Ctr) {
            if (ctr_x.C === Pair) {
               const [[g1, g2], cxsʹ]: [SVGElement[], PatternElement[]] = this.patterns(false, 2, cxs.slice(1))
               const [gsʹ, cxsʹʹ]: [SVGElement[], PatternElement[]] = this.patterns(parens, n - 1, cxsʹ)
               return [[this.parenthesise(this.horiz(g1, this.comma(ẟ_style), this.space(), g2), ẟ_style), ...gsʹ], cxsʹʹ]
            } else
            if (ctr_x.C === Nil || ctr_x.C === Cons) {
               const [g, cxsʹ]: [SVGElement, PatternElement[]] = this.listPattern(cxs[0], cxs.slice(1))
               const [gs, cxsʹʹ]: [SVGElement[], PatternElement[]] = this.patterns(parens, n - 1, cxsʹ)
               return [[g, ...gs], cxsʹʹ]
            } else {
               const [gs, cxsʹ]: [SVGElement[], PatternElement[]] = this.patterns(true, ctr_x.arity, cxs.slice(1))
               const g: SVGElement = this.horizSpace(this.text(ctr_x.c, ẟ_style), ...gs)
               const [gsʹ, cxsʹʹ]: [SVGElement[], PatternElement[]] = this.patterns(parens, n - 1, cxsʹ)
               return [[this.parenthesiseIf(ctr_x.arity > 0 && parens, g, ẟ_style), ...gsʹ], cxsʹʹ]
            }
         } else
         if (ctr_x instanceof Str) {
            const [gs, cxsʹ]: [SVGElement[], PatternElement[]] = this.patterns(parens, n - 1, cxs.slice(1))
            // ouch, ignore ẟ_style coming from trie and use variable instead :-/
            return [[this.patternVar(ctr_x), ...gs], cxsʹ]
         } else {
            return absurd()
         }
      }
   }

   patternVar (x: Str): SVGElement {
      return this.text(x.val, deltaStyle(x))
   }

   recDef (def: Expr.RecDef): SVGElement {
      return this.horizSpace(this.patternVar(def.x), this.elim(def.σ))
   }

   recDefₜ (def: Expl.RecDef): SVGElement {
      return this.horizSpace(this.patternVar(def.x), this.elim(def.tf.v.f))
   }

   space (): SVGElement {
      return this.text(`${space}`, DeltaStyle.Unchanged)
   }

   str (str: Str): SVGElement {
      return this.text(str.toString(), deltaStyle(str))
   }

   text (str: string, ẟ_style: DeltaStyle): SVGTextElement {
      const text: SVGTextElement = textElement(0, 0, fontSize, [classes, ẟ_style].join(" "), str)
      text.setAttribute("transform", `translate(${0},${lineHeight / 2})`)
      text.setAttribute("alignment-baseline", "central")
      const width: number = svg.textWidth(text)
      dimensions.set(text, { width, height: lineHeight })
      text.remove()
      return text
   }

   unimplemented (v: Value): SVGElement {
      // throw new Error(`TODO: ${className(v)}`)
      return this.text(`TODO: ${className(v)}`, DeltaStyle.Unchanged)
   }

   value (parens: boolean, v: Value): SVGElement {
      if (v instanceof Num) {
         return this.num(v)
      } else
      if (v instanceof Str) {
         return this.text(v.toString(), deltaStyle(v))
      } else
      if (v instanceof Eval.Closure) {
         return this.unimplemented(v)
      } else
      if (v instanceof DataValue) {
         if (v instanceof List) {
            return this.list(v)
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

   vert (...gs: SVGElement[]): SVGSVGElement {
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
