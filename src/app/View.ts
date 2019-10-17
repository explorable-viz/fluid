import { DataValue } from "../DataValue"
import { Expl } from "../Expl"
import { Id, Value, _ } from "../Value"
import { at } from "../Versioned"

const views: Map<Value, View> = new Map()

class View extends DataValue<"View"> {
}

class ExplValueView extends View {
   t: Expl = _
   v: Value = _
}

function explValueView (t: Expl, v: Value): (k: Id) => ExplValueView {
   return at(ExplValueView, t, v)
}

export class ExplView extends View {
}

export class ValueView extends View {
}

export function view (v: Value): View {
   let w: View | undefined = views.get(v)
   if (w === undefined) {
      w = new View()
      views.set(v, w)
      return w
   } else {
      return w
   }
}
