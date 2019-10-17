import { Class, __nonNull, absurd, as, assert, classOf } from "../util/Core"
import { Cons, List, Nil, Pair } from "../BaseTypes"
import { explClass } from "../DataType"
import { DataValue, ExplValue, explValue } from "../DataValue"
import { Eval } from "../Eval"
import { Expl } from "../Expl"
import { Expr } from "../Expr"
import { Elim, Match } from "../Match"
import { ApplicationId, Num, Str, TaggedId, Value } from "../Value"
import { newRevision, versioned } from "../Versioned"
import { ExprCursor } from "./Cursor"
import { Editor } from "./Editor"
import { 
   DeltaStyle, border, bracket, comma, deltaStyle, dimensions, ellipsis, horiz, horizSpace, keyword, parenthesise, space, text, unimplemented, vert 
} from "./Renderer2"

import Closure = Eval.Closure
import Cont = Expr.Cont

// Rather horrible idiom, but better than passing editors around everywhere.
let __editor: Editor | null = null

export class Renderer2 {
   render (tv: ExplValue, editor: Editor): [SVGElement, number] {
      __editor = editor
      const w: ExplValueView = view(tv) as ExplValueView
      w.showValue()
      const g: SVGElement = w.render()
      return [g, __nonNull(dimensions.get(g)).height]
   }
}

const views: Map<Value, View> = new Map()

function isExplFor (t: Expl, C: Class<DataValue>): boolean {
   return classOf(t) === explClass(C)
}

// Unpack evaluation memo-key to recover original expression.
function exprFor (t: Expl): Expr {
   if (versioned(t)) {
      return as(as(as(t.__id, TaggedId).k, ApplicationId).v, Expr.Expr)
   } else {
      return absurd()
   }
}

abstract class View {
   abstract render (): SVGElement
}

class ExplValueView extends View {
   tv: ExplValue
   ts_count: number // number of trace views to show (counting forward)
   v_visible: boolean

   assertValid (): void {
      assert(this.ts_count > 0 || this.v_visible)
   }

   constructor (tv: ExplValue) {
      super()
      this.tv = tv
      // initial view state:
      this.ts_count = 1
      this.v_visible = false
   }

   render (): SVGElement {
      this.assertValid()
      const [ts, tv]: [Expl[], ExplValue | null] = split(this.tv)
      if (ts.length === 0) {
         this.showValue()
         this.hideExpl()
      }
      const ts_g: SVGElement = vert(...ts.slice(0, this.ts_count).map(t => view(t).render()))
      let g: SVGElement 
      if (!this.v_visible) {
         g = ts_g
      } else
      if (this.ts_count === 0) {
         g = valueView(tv!).render()
      } else {
         g = horizSpace(ts_g, text("▸", DeltaStyle.Unchanged), valueView(tv!).render())
      }
      if (g instanceof SVGSVGElement) {
         return border(g)
      } else {
         return g
      }
   }

   // Probably want toggle _and_ count, but this will do for now.
   showExpl (): void {
      this.ts_count = 1
   }

   hideExpl (): void {
      if (this.v_visible) {
         this.ts_count = 0
      }
   }

   showValue (): void {
      this.v_visible = true
   }

   hideValue (): void {
      if (this.ts_count > 0) {
         this.v_visible = false
      }
   }
}

export class ExplView extends View {
   t: Expl

   constructor (t: Expl) {
      super()
      this.t = t
   }

   // TODO: reinstate parenthesisation
   render (): SVGElement {
      if (this.t instanceof Expl.Var) {
         return text(this.t.x.val, deltaStyle(this.t))
      }
      else
      if (this.t instanceof Expl.UnaryApp) {
         return view(this.t.tf).render()
      } else
      if (this.t instanceof Expl.BinaryApp) {
         return horizSpace(
            view(this.t.tv1).render(), 
            text(this.t.opName.val, deltaStyle(this.t)), // what about changes associated with t.opName? 
            view(this.t.tv2).render()
         )
      } else
      if (this.t instanceof Expl.App) {
         return horizSpace(view(this.t.tf).render(), view(this.t.tu).render())
      } else
      if (this.t instanceof Expl.Defs) {
         return vert(...this.t.def̅.toArray().map(defₜ))
      } else
      if (this.t instanceof Expl.MatchAs) {
         return vert(
            horizSpace(keyword("match", deltaStyle(this.t)), view(this.t.tu).render(), keyword("as", deltaStyle(this.t))),
            elimMatch(this.t.ξ)
         )
      } else {
         return absurd()
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

   render (): SVGElement {
      if (this.tv.v instanceof Num) {
         const e: Expr = exprFor(this.tv.t)
         return num(this.tv.v, e instanceof Expr.ConstNum ? e.val : undefined)
      } else
      if (this.tv.v instanceof Str) {
         return str(this.tv.v)
      } else
      if (this.tv.v instanceof DataValue) {
         if (isExplFor(this.tv.t, Pair)) {
            const vʹ: Pair = this.tv.v as Pair
            return pair(this.tv.t, Expl.explChild(this.tv.t, vʹ, "fst"), Expl.explChild(this.tv.t, vʹ, "snd"))
         } else
         if (isExplFor(this.tv.t, Nil) || isExplFor(this.tv.t, Cons)) {
            return list(this.tv)
         } else {
            return dataConstr(this.tv as ExplValue<DataValue>)
         }
      } else {
         return unimplemented(this.tv.v)
      }
   }
}

// Values are treated slightly differently because the "key" of a value view is the value (to distinguish
// it from the view of the ExplValue), but the Expl is also required to render the value.
export function valueView (tv: ExplValue): ValueView {
   let w: ValueView | undefined = views.get(tv.v) as ValueView
   if (w === undefined) {
      w = new ValueView(tv)
      views.set(tv.v, w)
      return w
   } else {
      return w
   }
}

export function view (v: ExplValue | Expl.Expl): View {
   let w: View | undefined = views.get(v)
   if (w === undefined) {
      if (v instanceof ExplValue) {
         w = new ExplValueView(v)
         views.set(v, w)
         return w
      } else
      if (v instanceof Expl.Expl) {
         w = new ExplView(v)
         views.set(v, w)
         return w
      } else {
         return absurd()
      }
   } else {
      return w
   }
}

// The value part must be an ExplValue, because in the data value case we need the explanation as well to
// render the value.
function split (tv: ExplValue): [Expl[], ExplValue | null] {
   const {t, v}: ExplValue = tv
   if (t instanceof Expl.Const) {
      return [[], tv]
   } else
   if (t instanceof Expl.DataExpl) {
      return [[], tv]
   } else
   if (t instanceof Expl.Var) {
      // values of variables themselves have explanations, but ignore those for now
      return [[t], v instanceof Closure ? null : tv]
   } else
   if (t instanceof Expl.UnaryApp || t instanceof Expl.BinaryApp) {
      return [[t], tv]
   } else
   if (t instanceof Expl.NonTerminal) {
      const [ts, vʹ] = split(explValue(t.t, v))
      return [[t, ...ts], vʹ]
   } else {
      return absurd()
   }
}

function dataConstr ({t, v}: ExplValue<DataValue>): SVGElement {
   const tvs: ExplValue[] = Expl.explChildren(t, v)
   // a constructor expression makes its value, so their root delta highlighting must agree
   return horizSpace(text(v.ctr, deltaStyle(v)), ...tvs.map(tvʹ => view(tvʹ).render()))
}

function defₜ (def: Expl.Def): SVGElement {
   if (def instanceof Expl.Prim) {
      return horizSpace(keyword("primitive", deltaStyle(def)), patternVar(def.x))
   } else
   if (def instanceof Expl.Let) {
      if (def.tv.t instanceof Expl.Const && def.tv.v instanceof Eval.Closure) {
         return horizSpace(keyword("let_", deltaStyle(def)), patternVar(def.x), elim(def.tv.v.f))
      } else {
         return horizSpace(
            keyword("let_", deltaStyle(def)), 
            patternVar(def.x), 
            keyword("equals", deltaStyle(def)),
            view(def.tv).render()
         )
      }
   } else
   if (def instanceof Expl.LetRec) {
      return horizSpace(keyword("letRec", deltaStyle(def)), vert(...def.δ.toArray().map(def => recDefₜ(def))))
   } else {
      return absurd()
   }
}

function elim<K extends Cont> (σ: Elim<K>): SVGElement {
   return unimplemented(σ)
}

function elimMatch<K extends Cont> (ξ: Match<K>): SVGElement {
   return unimplemented(ξ)
}

// Generalise to work with expressions?
function list ({t, v}: ExplValue): SVGElement {
   const gs: SVGElement[] = []
   while (isExplFor(t, Cons)) {
      const vʹ: Cons = v as Cons
      gs.push(view(Expl.explChild(t, vʹ, "head")).render())
      const {t: tʹ, v: vʹʹ}: ExplValue = Expl.explChild(t, vʹ, "tail")
      if (!(isExplFor(tʹ, Nil))) {
         // associate every Cons, apart from the last one, with a comma
         gs.push(comma(deltaStyle(t)), space())
      }
      t = tʹ
      v = as(vʹʹ, List)
   }
   if (isExplFor(t, Nil)) {
      return bracket(gs, deltaStyle(t))
   } else {
      // non-list expression in tail position determines delta-highlighting for brackets and ellipsis as well
      return bracket(
         [...gs, space(), ellipsis(deltaStyle(t)), view(explValue(t, v)).render()], 
         deltaStyle(t)
      )
   }
}

function num (n: Num, src?: Num): SVGElement {
   const g: SVGElement = text(n.toString(), deltaStyle(n))
   if (src && Number.isInteger(src.val)) {
      g.addEventListener("click", (ev: MouseEvent): void => {
         newRevision()
         new ExprCursor(src).setNum(src.val + 1)
         ev.stopPropagation()
         __editor!.onEdit()
      })
   }
   return g
}

// Will want to generalise this to deal with expressions.
function pair (t: Expl, tv1: ExplValue, tv2: ExplValue): SVGElement {
   return parenthesise(
      horiz(
         view(tv1).render(),
         comma(deltaStyle(t)),
         space(),
         view(tv2).render()
      ), 
      deltaStyle(t)
   )
}

function patternVar (x: Str): SVGElement {
   return text(x.val, deltaStyle(x))
}

function recDefₜ (def: Expl.RecDef): SVGElement {
   return horizSpace(patternVar(def.x), elim(def.tf.v.f))
}

function str (str: Str): SVGElement {
   return text(str.toString(), deltaStyle(str))
}
