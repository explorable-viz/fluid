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
   tws: ExplView[] | null
   vw: ValueView | null

   constructor (tv: ExplValue) {
      super()
      this.tv = tv
      // initial view state:
      this.tws = this.explViews()
      this.vw = null
   }

   render (): SVGElement {
      this.assertValid()
      let g: SVGElement 
      if (this.vw === null) {
         g = vert(...this.tws!.map(tw => tw.render()))
      } else
      if (this.tws === null) {
         g = this.vw!.render()
      } else {
         g = horizSpace(
            vert(...this.tws!.map(tw => tw.render())),
            text("▸", DeltaStyle.Unchanged), 
            this.vw.render()
         )
      }
      if (g instanceof SVGSVGElement) {
         return border(g)
      } else {
         return g
      }
   }

   explViews (): ExplView[] {
      return wurble(this.tv)[0].map(t => view(t) as ExplView)
   }

   toggleExpl (): void {
      if (this.tws === null) {
         this.tws = this.explViews()
      } else
      if (this.vw != null) {
         this.tws = null
      }
   }

   assertValid (): void {
      assert(this.tws !== null || this.vw !== null)
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
      } else {
         return notYetImplemented()
      }
   } else {
      return w
   }
}

function wurble ({t, v}: ExplValue): [Expl[], Value | null] {
   if (t instanceof Expl.Const) {
      return [[], v]
   } else
   if (t instanceof Expl.DataExpl) {
      return [[], v]
   } else
   if (t instanceof Expl.Var) {
      // values of variables themselves have explanations, but ignore those for now
      return [[t], v instanceof Closure ? null : v]
   } else
   if (t instanceof Expl.UnaryApp || t instanceof Expl.BinaryApp) {
      return [[t], v]
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
