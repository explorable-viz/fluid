import { Value } from "../Value"

const views: Map<Value, View> = new Map()

class View {
}

export class ExplValueView extends View {
   tw: ExplView
   vw: ValueView

   constructor (tw: ExplView, vw: ValueView) {
      super()
      this.tw = tw
      this.vw = vw
   }
}

export class ExplView extends View {
}

export class ValueView extends View {
}