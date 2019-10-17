import { assert, notYetImplemented } from "../util/Core"
import { DataValue, ExplValue } from "../DataValue"
import { Value } from "../Value"
import { DeltaStyle, horizSpace, text, vert } from "./Renderer2"

const views: Map<Value, View> = new Map()

abstract class View extends DataValue<"View"> {
}

class ExplValueView extends View {
   tw: ExplView | null = null
   vw: ValueView | null = null

   render (): SVGElement {
      assert(this.tw !== null || this.vw !== null)
      return horizSpace(vert(...gs), text("▸", DeltaStyle.Unchanged), g)
   }
}

export class ExplView extends View {
}

export class ValueView extends View {
}

export function view (v: Value): View {
   let w: View | undefined = views.get(v)
   if (w === undefined) {
      if (v instanceof ExplValue) {
         w = new ExplValueView()
         views.set(v, w)
         return w
      } else {
         return notYetImplemented()
      }
   } else {
      return w
   }
}
