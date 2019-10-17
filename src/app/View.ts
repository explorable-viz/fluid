import { Class, absurd, assert, classOf, notYetImplemented } from "../util/Core"
import { Pair } from "../BaseTypes"
import { explClass } from "../DataType"
import { DataValue, ExplValue, explValue } from "../DataValue"
import { Eval } from "../Eval"
import { Expl } from "../Expl"
import { Expr } from "../Expr"
import { Elim, Match } from "../Match"
import { Str, Value } from "../Value"
import { DeltaStyle, border, deltaStyle, horizSpace, keyword, text, unimplemented, vert } from "./Renderer2"

import Closure = Eval.Closure
import Cont = Expr.Cont

const views: Map<Value, View> = new Map()

function isExplFor (t: Expl, C: Class<DataValue>): boolean {
   return classOf(t) === explClass(C)
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
      const [ts, tv]: [Expl[], ExplValue | null] = wurble(this.tv)
      const ts_g: SVGElement = vert(...ts.slice(0, this.ts_count).map(t => view(t).render()))
      let g: SVGElement 
      if (!this.v_visible) {
         g = ts_g
      } else
      if (this.ts_count === 0) {
         g = view(tv!).render()
      } else {
         g = horizSpace(ts_g, text("▸", DeltaStyle.Unchanged), view(tv!).render())
      }
      if (g instanceof SVGSVGElement) {
         return border(g)
      } else {
         return g
      }
   }

   // Probably want toggle _and_ count, but this will do for now.
   toggleExpl (): void {
      if (this.ts_count === 0) {
         this.ts_count = 1
      } else
      if (this.v_visible) {
         this.ts_count = 0
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
   v: Value

   constructor (v: Value) {
      super()
      this.v = v
   }

   render (): SVGElement {
      return notYetImplemented()
   }
}

export function view (v: Value): View {
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
         w = new ValueView(v)
         views.set(v, w)
         return w
      }
   } else {
      return w
   }
}

// The value part must be an ExplValue, because in the data value case we need the explanation as well to
// render the value.
function wurble (tv: ExplValue): [Expl[], ExplValue | null] {
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
      const [ts, vʹ] = wurble(explValue(t.t, v))
      return [[t, ...ts], vʹ]
   } else {
      return absurd()
   }
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

function patternVar (x: Str): SVGElement {
   return text(x.val, deltaStyle(x))
}

function recDefₜ (def: Expl.RecDef): SVGElement {
   return horizSpace(patternVar(def.x), elim(def.tf.v.f))
}
