import { assert, notYetImplemented } from "../util/Core"
import { ExplValue } from "../DataValue"
import { Expl } from "../Expl"
import { Value } from "../Value"
import { DeltaStyle, border, horizSpace, text, vert } from "./Renderer2"

const views: Map<Value, View> = new Map()

abstract class View {
   abstract render (): SVGElement
}

class ExplValueView extends View {
   t: Expl
   v: Value
   tw: ExplView | null = null
   vw: ValueView | null = null

   constructor ({ t, v }: ExplValue) {
      super()
      this.t = t
      this.v = v
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
   render (): SVGElement {
      return vert(...gs)
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
