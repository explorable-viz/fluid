import { Class, absurd, assert, classOf, notYetImplemented } from "../util/Core"
import { Pair } from "../BaseTypes"
import { explClass } from "../DataType"
import { DataValue, ExplValue, explValue } from "../DataValue"
import { Eval } from "../Eval"
import { Expl } from "../Expl"
import { Value } from "../Value"
import { DeltaStyle, border, deltaStyle, horizSpace, text, vert } from "./Renderer2"

import Closure = Eval.Closure

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

   render (): SVGElement {
      if (this.t instanceof Expl.Var) {
         return text(this.t.x.val, deltaStyle(this.t))
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
