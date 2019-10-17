import { absurd, assert, notYetImplemented } from "../util/Core"
import { ExplValue, explValue } from "../DataValue"
import { Eval } from "../Eval"
import { Expl } from "../Expl"
import { Value } from "../Value"
import { DeltaStyle, border, horizSpace, text, vert } from "./Renderer2"

import Closure = Eval.Closure

const views: Map<Value, View> = new Map()

abstract class View {
   abstract render (): SVGElement
}

class ExplValueView extends View {
   tv: ExplValue
   tw: ExplView | null
   vw: ValueView | null

   constructor (tv: ExplValue) {
      super()
      this.tv = tv
      // initial view state:
      this.tw = view(this.t)
      this.vw = null
   }

   render (): SVGElement {
      this.assertValid()
      let g: SVGElement 
      if (this.vw === null) {
         g = this.tw!.render()
      } else
      if (this.tw === null) {
         g = this.vw!.render()
      } else {
         g = horizSpace(
            this.tw.render(), 
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

   toggleExpl (): void {
      if (this.tw === null) {
         this.tw = view(this.t)
      } else
      if (this.vw != null) {
         this.tw = null
      }
   }

   assertValid (): void {
      assert(this.tw !== null || this.vw !== null)
   }
}

export class ExplView extends View {
   t: Expl

   constructor (t: Expl) {
      super()
      this.t = t
   }

   render (): SVGElement {

   }
}

export class ExplsView extends View {
   ts: Expl[]
   tws: ExplView[] // not every expl need have a view

   constructor (ts: Expl[]) {
      super()
      this.ts = ts
      // initial view state:
      this.tws = [view(ts[0]) as ExplView]
   }

   render (): SVGElement {
      return vert(...this.tws.map(tw => tw.render()))
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
