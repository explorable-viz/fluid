import { DataValue, ExplValue } from "../DataValue"
import { Id, MemoId, _, memoId } from "../Value"
import { at } from "../Versioned"

class View extends DataValue<"View"> {
}

class ExplValueView extends View {
   tw: ExplView = _
   vw: ValueView = _
}

function explValueView (tw: ExplView, vw: ValueView): (k: Id) => ExplValueView {
   return at(ExplValueView, tw, vw)
}

export class ExplView extends View {
}

export class ValueView extends View {
}

export function view (tv: ExplValue): ExplValueView {
   const k: MemoId = memoId(view, arguments)
   // make tw, vw
   return explValueView(tw, vw)(k)
}
