import { DataValue, ExplValue } from "../DataValue"
import { Expl } from "../Expl"
import { Id, MemoId, Value, _, memoId } from "../Value"
import { at } from "../Versioned"

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

export function view ({ t, v }: ExplValue): ExplValueView {
   const k: MemoId = memoId(view, arguments)
   return explValueView(t, v)(k)
}
