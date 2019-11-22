import { Class, __nonNull, absurd, as, assert, className, classOf } from "../util/Core"
import { flatten, nth, zip } from "../util/Array"
import { Cons, List, Nil, Pair } from "../BaseTypes"
import { Ctr, ctrFor, exprClass } from "../DataType"
import { DataValue, ExplValue, explValue } from "../DataValue"
import { Change } from "../Delta"
import { Env } from "../Env"
import { Eval } from "../Eval"
import { Expl } from "../Expl"
import { Expr } from "../Expr"
import { GraphicsElement } from "../Graphics2"
import { DataElim, Elim, Match, VarElim } from "../Match"
import { Module, parseWithImports2 } from "../Module"
import { ApplicationId, Num, Str, TaggedId, Value, fields } from "../Value"
import { ν, at, newRevision, num, str, versioned } from "../Versioned"
import { ExprCursor } from "./Cursor"
import { Editor } from "./Editor"
import { Interactor } from "./Interactor"
import { GraphicsRenderer } from "./GraphicsRenderer2"
import { 
   DeltaStyle, arrow, addBorder_changed, addBorder_focus, centreDot, comma, connector, deltaStyle, __dimensions, ellipsis, horiz, 
   horizSpace, keyword, edge_left, parenthesise, parenthesiseIf, shading, space, svgElement_inverted, text, unimplemented, vert 
} from "./Renderer"

import Closure = Eval.Closure
import Cont = Expr.Cont

export module View {
   export let dimensions: (tg: ExplValue<GraphicsElement>) => [number, number]

   // Shenanigans to call an internal function. Will extract this into a (reverse) FFI.
   export function initialise (): void {
      Module.initialise()
      const x: string = "g"
      const [ρ, dimsExpr]: [Env, Expr] = parseWithImports2(`dimensions ${x}`)

      dimensions = function (tg: ExplValue<GraphicsElement>): [number, number] {
         const tv: ExplValue = Eval.eval_(ρ.concat(Env.singleton(str(x)(ν()), tg)), dimsExpr)
         if (tv.v instanceof Pair && tv.v.fst instanceof Num && tv.v.snd instanceof Num) {
            return [tv.v.fst.val, tv.v.snd.val]
         } else {
            return absurd()
         }
      }
   }

   // Prefer globals to threading parameters everywhere :-o
   let __editor: Editor.Editor | null = null

   type Link = {
      from: View, 
      to: View
   }

   const __links: Set<Link> = new Set()
   const __svgs: Map<View, SVGSVGElement> = new Map() // memoised render within a single update 

   export function render (root: SVGSVGElement, tv: ExplValue, editor: Editor.Editor): void {
      __svgs.clear()
      __links.clear()
      __editor = editor
      const g: SVGElement = view(tv, true, true).render()
      root.appendChild(g) // need to render the main view so links can make use of getBoundingClientRect
      renderLinks(__links).forEach((link: SVGElement): void => {
         root.appendChild(link)
      })
   }

   const views: Map<Value, View> = new Map() // persists across edits

   export function existingView (tv: ExplValue): ExplValueView {
      return __nonNull(views.get(tv)) as ExplValueView
   }

   function isExprFor (e: Expr, C: Class<DataValue>): boolean {
      return classOf(e) === exprClass(C)
   }

   // Unpack evaluation memo-key to recover original expression. TODO: make generic
   // and move near to memo code.
   function exprFor (t: Expl): Expr {
      if (versioned(t)) {
         return as(as(as(t.__id, TaggedId).k, ApplicationId).v, Expr.Expr)
      } else {
         return absurd()
      }
   }

   function renderLinks (links: Set<Link>): SVGElement[] {
      return [...links].map(((link: Link): SVGElement => {
         return connector(__nonNull(__svgs.get(link.from)), __nonNull(__svgs.get(link.to)))
      }))
   }

   abstract class View {
      render (): SVGSVGElement {
         const g: SVGSVGElement = this.render_()
         __svgs.set(this, g)
         return g
      }
      abstract render_ (): SVGSVGElement
   }

   class ExprView extends View {
      parens: boolean
      e: Expr

      constructor (parens: boolean, e: Expr) {
         super()
         this.parens = parens
         this.e = e
      }

      render_ (): SVGSVGElement {
         const parens: boolean = this.parens
         const e: Expr = this.e
         if (e instanceof Expr.ConstNum) {
            // ouch: disregard delta-info on expression itself
            return horiz(num_(e.val, e.val))
         } else
         if (e instanceof Expr.ConstStr) {
            // ouch: disregard delta-info on expression itself
            return horiz(str_(e.val))
         } else
         if (e instanceof Expr.Fun) {
            const g: SVGSVGElement = horizSpace(keyword("fun", deltaStyle(e)), elim(e.σ))
            return parenthesiseIf(parens, g, deltaStyle(e))
         } else
         if (e instanceof Expr.DataExpr) {
            if (isExprFor(e, Pair)) {
               return pair_expr(e)
            } else
            if (isExprFor(e, Nil) || isExprFor(e, Cons)) {
               return list_expr(parens, e)
            } else {
               return dataConstr_expr(parens, e)
            }
         } else
         if (e instanceof Expr.Quote) {
            return unimplemented(e)
         } else
         if (e instanceof Expr.Var) {
            // ouch: disregard delta-info on Var.x
            return horiz(text(e.x.val, deltaStyle(e)))
         } else
         if (e instanceof Expr.App) {
            return parenthesiseIf(
               parens, 
               horizSpace(expr(!(e.f instanceof Expr.App), e.f), expr(true, e.e)),
               deltaStyle(e)
            )
         } else
         if (e instanceof Expr.BinaryApp) {
            // ignore operator precedence, but allow function application to take priority over any binary operation
            return parenthesiseIf(
               parens, 
               horizSpace(
                  expr(!(e.e1 instanceof Expr.App), e.e1), 
                  text(e.opName.val, deltaStyle(e)), // what about changes associated with e.opName 
                  expr(!(e.e2 instanceof Expr.App), e.e2)
               ),
               deltaStyle(e)
            )
         } else
         if (e instanceof Expr.Defs) {
            return parenthesiseIf(
               parens,
               vert(
                  vert(...e.def̅.toArray().map(def_ => def(def_))),
                  expr(false, e.e)
               ),
               deltaStyle(e)
            )
         } else
         if (e instanceof Expr.MatchAs) {
            return vert(
               horizSpace(keyword("match", deltaStyle(e)), expr(false, e.e), keyword("as", deltaStyle(e))),
               elim(e.σ)
            )
         } else
         if (e instanceof Expr.Typematch) {
            return vert(
               horizSpace(keyword("typematch", deltaStyle(e)), expr(false, e.e), keyword("as", deltaStyle(e))),
               ...e.cases.toArray().map(({fst: x, snd: e}: Pair<Str, Expr>) => 
                  horizSpace(text(x.val, deltaStyle(x)), arrow(deltaStyle(e)), expr(false, e))
               )
            )
         } else {
            return absurd(`Unimplemented expression form: ${className(e)}.`)
         }
      }
   }

   class ExplValueView extends View {
      tv: ExplValue
      show_v: boolean
      show_ts: boolean
      t_visible!: boolean
      v_visible!: boolean

      assertValid (): void {
         assert(this.show_v || this.show_ts)
      }

      constructor (tv: ExplValue, show_v: boolean, show_ts: boolean) {
         super()
         this.tv = tv
         this.show_v = show_v
         this.show_ts = show_ts
         this.initialise()
      }

      initialise (): [Expl[], ExplValue | null] {
         const ts: Expl[] = splitExpls(this.tv.t)
         if (ts.length === 0 || !this.show_ts) {
            this.t_visible = false
            this.v_visible = true
         } else {
            this.t_visible = true
            this.v_visible = this.show_v
         }
         return [ts, splitValue(this.tv)]
      }

      render_ (): SVGSVGElement {
         this.assertValid()
         const [ts, tv]: [Expl[], ExplValue | null] = this.initialise()
         let g: SVGSVGElement 
         if (!this.v_visible) {
            g = expls(ts)
         } else
         if (!this.t_visible) {
            g = valueView(tv!).render()
         } else {
            g = vert(expls(ts), horizSpace(text("▸", deltaStyle(nth(ts, ts.length - 1))), valueView(tv!).render()))
         }
         if (this.tv === __editor!.here.tv) {
            return addBorder_focus(!this.t_visible && ts.length > 0  ? edge_left(g) : g)
         } else {
            return g
         }
      }

      toggleValue (): void {
         if (!this.show_v) {
            this.show_v = true
         } else
         if (this.show_ts) {
            this.show_v = false
         }
      }

      toggleExpl (): void {
         if (!this.show_ts) {
            this.show_ts = true
         } else
         if (this.show_v) {
            this.show_ts = false
         }
      }
   }

   export class ExplView extends View {
      t: Expl
      bodyVisible: boolean // if I am an application view, whether the function body is visible

      constructor (t: Expl) {
         super()
         this.t = t
         this.bodyVisible = false
      }

      render_ (): SVGSVGElement {
         let g: SVGSVGElement
         if (this.t instanceof Expl.Var) {
            g = horiz(text(this.t.x.val, deltaStyle(this.t)))
         }
         else
         if (this.t instanceof Expl.UnaryApp) {
            g = view(this.t.tf, false, true).render()
         } else
         if (this.t instanceof Expl.BinaryApp) {
            g = horizSpace(
               view(this.t.tv1, false, true).render(), 
               text(this.t.opName.val, deltaStyle(this.t)), // what about changes associated with t.opName? 
               view(this.t.tv2, false, true).render()
            )
         } else
         if (this.t instanceof Expl.App) {
            g = vert(
               horizSpace(view(this.t.tf, false, true).render(), view(this.t.tu, false, true).render()),
               this.appBody()
            )
         } else
         if (this.t instanceof Expl.Defs) {
            g = vert(...this.t.def̅.toArray().map(defₜ))
         } else
         if (this.t instanceof Expl.MatchAs) {
            g = vert(
               horizSpace(keyword("match", deltaStyle(this.t)), view(this.t.tu, false, true).render(), keyword("as", deltaStyle(this.t))),
               elimMatch(this.t.ξ)
            )
         } else
         if (this.t instanceof Expl.Typematch) {
            return unimplemented(this.t)
         } else {
            return absurd("Unknown explanation form", this.t)
         }
         return shading(g, "white")
      }

      appBody (): SVGElement {
         const app: Expl.App = as(this.t, Expl.App)
         const ts: Expl[] = splitExpls(app.t)
         if (ts.length === 0 || this.bodyVisible) {
            return expls(ts)
         } else {
            const g: SVGElement = ellipsis(deltaStyle(app.t))
            g.addEventListener("click", (ev: MouseEvent): void => {
               ev.stopPropagation()
               this.bodyVisible = true
               __editor!.onViewChange()
            })
            return g
         }
      }
   }

   export class ValueView extends View {
      // We need the "leaf" explanation to render a value, for two reasons: so we can retrieve the original expression for 
      // editing purposes, and to render component explanations of data values.
      tv: ExplValue

      constructor (tv: ExplValue) {
         super()
         this.tv = tv
      }

      render_ (): SVGSVGElement {
         let g: SVGSVGElement
         if (this.tv.v instanceof Num) {
            const e: Expr = exprFor(this.tv.t)
            g = horiz(num_(this.tv.v, e instanceof Expr.ConstNum ? e.val : undefined))
         } else
         if (this.tv.v instanceof Str) {
            g = horiz(str_(this.tv.v))
         } else
         if (this.tv.v instanceof Closure) {
            // treat closures as their function literals, for now
            g = horizSpace(keyword("fun", deltaStyle(this.tv.v)), elim(this.tv.v.f))
         } else
         if (this.tv.v instanceof DataValue) {
            if (this.tv.v instanceof GraphicsElement) {
               const tg: ExplValue<GraphicsElement> = this.tv as ExplValue<GraphicsElement>
               const dim = { width: 480, height: 480 }
               let g1: SVGGElement
               [g, g1] = svgElement_inverted(dim.width, dim.height)
               new GraphicsRenderer(new Interactor(), g, g1).render(tg, __nonNull(dimensions)(tg))
               __dimensions.set(g, dim)
            } else
            if (this.tv.v instanceof Pair) {
               g = pair(this.tv as ExplValue<Pair>)
            } else
            if (this.tv.v instanceof List) {
               g = list(this.tv as ExplValue<List>)
            } else {
               g = dataConstr(false, this.tv as ExplValue<DataValue>)
            }
         } else {
            g = unimplemented(this.tv.v)
         }
         return shading(g, "lavender")
      }
   }

   // Values are treated slightly differently because the "key" of a value view is the value (to distinguish
   // it from the view of the ExplValue), but the Expl is also required to render the value.
   export function valueView (tv: ExplValue): ValueView {
      let w: ValueView | undefined = views.get(__nonNull(tv).v) as ValueView
      if (w === undefined) {
         w = new ValueView(tv)
         views.set(tv.v, w)
         return w
      } else {
         return w
      }
   }

   export function view (tv: ExplValue, show_v: boolean, show_ts: boolean): ExplValueView {
      let w: ExplValueView | undefined = views.get(tv) as ExplValueView
      if (w === undefined) {
         w = new ExplValueView(tv, show_v, show_ts)
         views.set(tv, w)
         return w
      } else {
         return w
      }
   }

   function view_child<T extends DataValue> (C: Class<T>, tv: ExplValue<T>, prop_: keyof T, show_v: boolean, show_ts: boolean): SVGSVGElement {
      if (versioned(tv.v) && versioned(tv.t)) {
         const prop: string = prop_ as string
         const w: View = view(Expl.explChild(tv.t, tv.v, prop_), show_v, show_ts)
         const g: SVGSVGElement = w.render()
         if (tv.v.__ẟ instanceof Change && tv.v.__ẟ.hasChanged(prop as string)) {
            // All a bit hacky, need to rethink:
            const t_prev: Expl = 
               tv.t.__ẟ instanceof Change && tv.t.__ẟ.hasChanged(prop as string) ? 
               as(tv.t.__ẟ.changed[prop].before, Expl.Expl) :
               tv.t
            const w_existing: View | undefined = views.get(explValue(t_prev, as(tv.v.__ẟ.changed[prop].before, Value)))
            if (w_existing) {
               __links.add({ from: w, to: w_existing })
            }
            return addBorder_changed(g)
         } else {
            return g
         }
      } else {
         return absurd()
      }
   }

   export function explView (t: Expl.Expl): View {
      let w: ExplView | undefined = views.get(t) as ExplView
      if (w === undefined) {
         w = new ExplView(t)
         views.set(t, w)
         return w
      } else {
         return w
      }
   }

   export function splitExpls (t: Expl): Expl[] {
      if (t instanceof Expl.Const) {
         return []
      } else
      if (t instanceof Expl.Fun) {
         return []
      } else
      if (t instanceof Expl.DataExpl) {
         return []
      } else
      if (t instanceof Expl.Var) {
         // values of variables themselves have explanations, but ignore those for now
         return [t]
      } else
         // don't recurse into App as it has its own expansion state
      if (t instanceof Expl.UnaryApp || t instanceof Expl.BinaryApp) {
         return [t]
      } else
      if (t instanceof Expl.App) {
         return [t]
      } else
      if (t instanceof Expl.NonTerminal) {
         return [t, ...splitExpls(t.t)]
      } else {
         return absurd("Unknown explanation form", t)
      }
   }

   // The value part must be an ExplValue, because in the data value case we need the explanation as well to
   // render the value.
   export function splitValue (tv: ExplValue): ExplValue {
      const {t, v}: ExplValue = tv
      if (t instanceof Expl.Const) {
         return tv
      } else
      if (t instanceof Expl.Fun) {
         return tv
      } else
      if (t instanceof Expl.DataExpl) {
         return tv
      } else
      if (t instanceof Expl.Var) {
         // values of variables themselves have explanations, but ignore those for now
         return splitValue(explValue(t.t, v))
      } else
         // don't recurse into App as it has its own expansion state
      if (t instanceof Expl.UnaryApp || t instanceof Expl.BinaryApp) {
         return tv
      } else
      if (t instanceof Expl.App) {
         return tv
      } else
      if (t instanceof Expl.NonTerminal) {
         return splitValue(explValue(t.t, v))
      } else {
         return absurd()
      }
   }

   function expls (ts: Expl[]): SVGSVGElement {
      return vert(...ts.map(t => explView(t).render()))
   }

   // To visualise an eliminator, we reconstruct the patterns from the trie. List syntax in particular doesn't have
   // an analogous "case tree" form.
   type PatternElement = [Ctr | Str, DeltaStyle]

   function compareCtr (c1: string, c2: string): number {
      const n: number = ctrFor(c1).arity - ctrFor(c2).arity
      return n === 0 ? c1.localeCompare(c2) : n
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

   function consComma (ẟ_style: DeltaStyle, src?: Expr.DataExpr): SVGElement {
      const g: SVGElement = comma(ẟ_style)
      g.addEventListener("click", (ev: MouseEvent): void => {
         ev.stopPropagation()
         if (src !== undefined) {
            newRevision()
            if (ev.metaKey) {
               if (ev.altKey) {
                  // if my tail is another cons, swap the two head elements
                  const e: Expr = as(new ExprCursor(src).constr_to(Cons, "tail").v, Expr.Expr)
                  if (isExprFor(e, Cons)) {
                     const e1: Expr = as(new ExprCursor(src).constr_to(Cons, "head").v, Expr.Expr)
                     const e2: Expr = as(new ExprCursor(e).constr_to(Cons, "head").v, Expr.Expr)
                     // constr_splice on src, replacing head with head of src.tail
                     // constr_splice on src.tail, replacing head with head of src
                     new ExprCursor(src).constr_splice(Cons, ["head"], ([e]: Expr[]): Expr[] => {
                        return [e2]
                     })
                     new ExprCursor(e).constr_splice(Cons, ["head"], ([e]: Expr[]): Expr[] => {
                        return [e1]
                     })
                  }
               } else {
                  new ExprCursor(src).constr_splice(Cons, ["head"], ([e]: Expr[]): Expr[] => {
                     const eʹ: Expr = Expr.app(Expr.var_(str("sq")(ν()))(ν()), Expr.var_(str("x")(ν()))(ν()))(ν())
                     return [at(exprClass(Pair), e, eʹ)(ν())]
                  })
            }
            } else {
               new ExprCursor(src).constr_splice(Cons, ["tail"], ([e]: Expr[]): Expr[] => {
                  const eʹ: Expr = Expr.constNum(num(0)(ν()))(ν())
                  return [at(exprClass(Cons), eʹ, e)(ν())]
               })
            }
            __editor!.onEdit()
         }
      })
      return g
   }

   function dataConstr (parens: boolean, {t, v}: ExplValue<DataValue>): SVGSVGElement {
      const tvs: ExplValue[] = Expl.explChildren(t, v)
      // a constructor expression makes its value, so their root delta highlighting must agree
      const gs: SVGSVGElement[] = tvs.map(tvʹ => view(tvʹ, true, false).render())
      const g: SVGSVGElement = horizSpace(text(v.ctr, deltaStyle(v)), ...(tvs.length > 2 ? [vert(...gs)] : gs))
      return parenthesiseIf(tvs.length > 0 && parens, g, deltaStyle(t))
   }

   function dataConstr_expr (parens: boolean, e: Expr.DataExpr): SVGSVGElement {
      const es: Expr[] = e.__children
      const gs: SVGSVGElement[] = es.map(eʹ => expr(true, eʹ))
      const g: SVGSVGElement = horizSpace(text(e.ctr, deltaStyle(e)), ...(es.length > 2 ? [vert(...gs)] : gs))
      return parenthesiseIf(es.length > 0 && parens, g, deltaStyle(e))
   }

   function def (def: Expr.Def): SVGSVGElement {
      if (def instanceof Expr.Prim) {
         return horizSpace(keyword("primitive", deltaStyle(def)), patternVar(def.x))
      } else
      if (def instanceof Expr.Let) {
         if (def.e instanceof Expr.Fun) {
            return horizSpace(keyword("let_", deltaStyle(def)), patternVar(def.x), elim(def.e.σ))
         } else {
            return horizSpace(
               keyword("let_", deltaStyle(def)), 
               patternVar(def.x), 
               keyword("equals", deltaStyle(def)), 
               expr(false, def.e)
            )
         }
      } else
      if (def instanceof Expr.LetRec) {
         return horizSpace(keyword("letRec", deltaStyle(def)), vert(...def.δ.toArray().map(def => recDef(def))))
      } else {
         return absurd()
      }
   }

   function defₜ (def: Expl.Def): SVGSVGElement {
      if (def instanceof Expl.Prim) {
         return horizSpace(keyword("primitive", deltaStyle(def)), patternVar(def.x))
      } else
      if (def instanceof Expl.Let) {
         if (def.tv.t instanceof Expl.Fun && def.tv.v instanceof Closure) {
            return horizSpace(keyword("let_", deltaStyle(def)), patternVar(def.x), elim(def.tv.v.f))
         } else {
            return horizSpace(
               keyword("let_", deltaStyle(def)), 
               patternVar(def.x), 
               keyword("equals", deltaStyle(def)),
               view(def.tv, false, true).render()
            )
         }
      } else
      if (def instanceof Expl.LetRec) {
         return horizSpace(keyword("letRec", deltaStyle(def)), vert(...def.δ.toArray().map(def => recDefₜ(def))))
      } else {
         return absurd()
      }
   }

   function elim<K extends Cont> (σ: Elim<K>): SVGSVGElement {
      return vert(...clauses(σ).map(([cxs, e]) => {
         const [[g], cxsʹ]: [SVGElement[], PatternElement[]] = patterns(false, 1, cxs)
         assert(cxsʹ.length === 0)
         const gʹ: SVGElement = 
            e instanceof Expr.Fun ?
            elim(e.σ) : // curried function resugaring
            horizSpace(arrow(deltaStyle(e)), expr(false, e))
         return horizSpace(g, gʹ)
      }))
   }

   // Hack just to support Bool, Ordering, etc.
   function elimMatch<K extends Cont> (ξ: Match<K>): SVGSVGElement {
      const tv: ExplValue<DataValue> = nth(ξ.tv̅.toArray(), 0)
      // don't think the contination is needed; already stored in the trace
      return horizSpace(text(tv.v.ctr, deltaStyle(tv.v)), arrow(deltaStyle(tv.v)))
   }

   function expr_ (parens: boolean, e: Expr): ExprView {
      let w: ExprView | undefined = views.get(e) as ExprView
      if (w === undefined) {
         w = new ExprView(parens, e)
         views.set(e, w)
         return w
      } else {
         return w
      }
   }

   function expr (parens: boolean, e: Expr): SVGSVGElement {
      return expr_(parens, e).render()
   }

   // Really want some kind of view typeclass, so this isn't specific to expression. Also: consolidate with ExprCursor.
   function expr_child<T extends DataValue> (C: Class<T>, parens: boolean, e: Expr.DataExpr, prop: keyof T): SVGElement {
      if (versioned(e)) {
         const w: ExprView = expr_(parens, e.__child(prop as string))
         const g: SVGSVGElement = w.render()
         if (e.__ẟ instanceof Change && e.__ẟ.hasChanged(prop as string)) {
            const w_existing: View | undefined = views.get(as(e.__ẟ.changed[prop as string].before, Expr.Expr))
            if (w_existing) {
               __links.add({ from: w, to: w_existing })
            }
            return addBorder_changed(g)
         } else {
            return g
         }
      } else {
         return absurd()
      }   
   }

   function list ({t, v}: ExplValue<List>): SVGSVGElement {
      if (Cons.is(v)) {
         const vʹ: Cons = v as Cons
         const e: Expr = exprFor(t)
         return horiz(
            view_child(Cons, explValue(t, vʹ), "head", true, false),
            consComma(deltaStyle(v), isExprFor(e, Cons) ? e as Expr.DataExpr : undefined),
            space(),
            view(Expl.explChild(t, vʹ, "tail"), true, false).render()
         )
      } else
      if (Nil.is(v)) {
         return horiz(centreDot(deltaStyle(v)))
      } else {
         return absurd()
      }
   }

   function list_expr (parens: boolean, e: Expr.DataExpr): SVGSVGElement {
      if (isExprFor(e, Cons)) {
         return parenthesiseIf(parens, 
            horiz(
               expr_child(Cons, false, e, "head"),
               consComma(deltaStyle(e), e),
               space(), 
               list_expr(false, e.__child("tail") as Expr.DataExpr)
            ),
            deltaStyle(e)
         )
      } else
      if (isExprFor(e, Nil)) {
         return horiz(centreDot(deltaStyle(e)))
      } else {
         return horiz(expr(false, e)) // promote to nested SVG; need to rethink
      }
   }

   function num_ (n: Num, src?: Num): SVGElement {
      const g: SVGElement = text(n.toString(), deltaStyle(n))
      if (src && Number.isInteger(src.val)) {
         g.addEventListener("click", (ev: MouseEvent): void => {
            newRevision()
            new ExprCursor(src).setNum(ev.metaKey ? src.val - 1 : src.val + 1)
            ev.stopPropagation()
            __editor!.onEdit()
         })
      }
      return g
   }

   function pair (tv: ExplValue<Pair>): SVGSVGElement {
      return parenthesise(
         horiz(
            view_child(Pair, tv, "fst", true, false),
            pairComma(deltaStyle(tv.t), exprFor(tv.t) as Expr.DataExpr),
            space(),
            view_child(Pair, tv, "snd", true, false)
         ), 
         deltaStyle(tv.t)
      )
   }

   function pairComma (ẟ_style: DeltaStyle, src?: Expr.DataExpr): SVGElement {
      const g: SVGElement = comma(ẟ_style)
      g.addEventListener("click", (ev: MouseEvent): void => {
         ev.stopPropagation()
         if (src !== undefined) {
            newRevision()
            if (ev.metaKey) {
               new ExprCursor(src).constr_splice(Pair, ["fst", "snd"], ([e1, e2]: Expr[]): Expr[] => {
                  return [e2, e1]
               })
            }
            __editor!.onEdit()
         }
      })
      return g
   }

   function pair_expr (e: Expr.DataExpr): SVGSVGElement {
      return parenthesise(
         horiz(
            expr_child(Pair, false, e, "fst"),
            pairComma(deltaStyle(e), e),
            space(),
            expr_child(Pair, false, e, "snd")
         ), 
         deltaStyle(e)
      )
   }
   function patterns (parens: boolean, n: number, cxs: PatternElement[]): [SVGElement[], PatternElement[]] {
      if (n === 0) {
         return [[], cxs]
      } else {
         const [ctr_x, ẟ_style] = cxs[0]
         if (ctr_x instanceof Ctr) {
            if (ctr_x.C === Pair) {
               const [[g1, g2], cxsʹ]: [SVGElement[], PatternElement[]] = patterns(false, 2, cxs.slice(1))
               const [gsʹ, cxsʹʹ]: [SVGElement[], PatternElement[]] = patterns(parens, n - 1, cxsʹ)
               return [[parenthesise(horiz(g1, comma(ẟ_style), space(), g2), ẟ_style), ...gsʹ], cxsʹʹ]
            } else
            if (ctr_x.C === Nil) {
               const [g, cxsʹ]: [SVGElement, PatternElement[]] = [centreDot(ẟ_style), cxs.slice(1)]
               const [gs, cxsʹʹ]: [SVGElement[], PatternElement[]] = patterns(parens, n - 1, cxsʹ)
               return [[g, ...gs], cxsʹʹ]
            } else
            if (ctr_x.C === Cons) {
               const [[g_head, g_tail], cxsʹ]: [SVGElement[], PatternElement[]] = patterns(false, ctr_x.arity, cxs.slice(1))
               const g: SVGSVGElement = horiz(g_head, comma(ẟ_style), space(), g_tail)
               const [gsʹ, cxsʹʹ]: [SVGElement[], PatternElement[]] = patterns(parens, n - 1, cxsʹ)
               return [[parenthesiseIf(ctr_x.arity > 0 && parens, g, ẟ_style), ...gsʹ], cxsʹʹ]
            } else {
               const [gs, cxsʹ]: [SVGElement[], PatternElement[]] = patterns(true, ctr_x.arity, cxs.slice(1))
               const g: SVGSVGElement = horizSpace(text(ctr_x.c, ẟ_style), ...gs)
               const [gsʹ, cxsʹʹ]: [SVGElement[], PatternElement[]] = patterns(parens, n - 1, cxsʹ)
               return [[parenthesiseIf(ctr_x.arity > 0 && parens, g, ẟ_style), ...gsʹ], cxsʹʹ]
            }
         } else
         if (ctr_x instanceof Str) {
            const [gs, cxsʹ]: [SVGElement[], PatternElement[]] = patterns(parens, n - 1, cxs.slice(1))
            // ouch, ignore ẟ_style coming from trie and use variable instead :-/
            return [[patternVar(ctr_x), ...gs], cxsʹ]
         } else {
            return absurd()
         }
      }
   }

   function patternVar (x: Str): SVGElement {
      return text(x.val, deltaStyle(x))
   }

   function recDef (def: Expr.RecDef): SVGSVGElement {
      return horizSpace(patternVar(def.x), elim(def.σ))
   }

   function recDefₜ (def: Expl.RecDef): SVGSVGElement {
      return horizSpace(patternVar(def.x), elim(def.tf.v.f))
   }

   function str_ (str: Str): SVGElement {
      return text(str.toString(), deltaStyle(str))
   }
}
