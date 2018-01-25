import { assert } from "./Util"
import { Bool, Int, Prim, Str } from "./BaseTypes"
import { ITraced, create, typeCheck_ } from "./Runtime"

// Used to have root views as a separate data type but easier for now just to combine.
export class View {
   __visit <T> (v: ViewVisitor<T>): T {
      return assert(false)
   }
}

export class ViewVisitor<T> {
   is_EmptyView (w: EmptyView): T { return assert(false) }
   is_Space (w: Space): T { return assert(false) }
   is_Background (w: Background): T { return assert(false) }
   is_RoundedCell (w: RoundedCell): T { return assert(false) }
   is_Word (w: Word): T { return assert(false) }
   is_Vert (w: Vert): T { return assert(false) }
   is_Horiz (w: Horiz): T { return assert(false) }

}
export class EmptyView extends View {
   static at (α: Addr): EmptyView {
      const this_: EmptyView = create(α, EmptyView)
      this_.__version()
      return this_
   }

   __visit<T> (v: ViewVisitor<T>): T {
      return v.is_EmptyView(this)
   }
}

export class Space extends View {
   static at (α: Addr): Space {
      const this_: Space = create(α, Space)
      this_.__version()
      return this_
   }

   __visit<T> (v: ViewVisitor<T>): T {
      return v.is_Space(this)
   }
}

export class Background extends View {
   _greyScale: ITraced<Int>
   _child: ITraced<View>

   static at (α: Addr, greyScale: ITraced<Int>, child: ITraced<View>): Background {
      const this_: Background = create(α, Background)
      this_._greyScale = greyScale
      this_._child = typeCheck_(child, View)
      this_.__version()
      return this_
   }

   __visit<T> (v: ViewVisitor<T>): T {
      return v.is_Background(this)
   }

   get greyScale (): Int {
      return this._greyScale.val
   }

   get child (): View {
      return this._child.val
   }
}

// My sub-view must be non-empty.
export class RoundedCell extends View {
   _borderGreyScaleOpt: ITraced<Prim.Option<Int>>
   _child: ITraced<View>

   static at (α: Addr, borderGreyScaleOpt: ITraced<Prim.Option<Int>>, child: ITraced<View>): RoundedCell {
      const this_: RoundedCell = create(α, RoundedCell)
      this_._borderGreyScaleOpt = typeCheck_(borderGreyScaleOpt, Prim.Option)
      this_._child = typeCheck_(child, View)
      this_.__version()
      return this_
   }

   __visit<T> (v: ViewVisitor<T>): T {
      return v.is_RoundedCell(this)
   }

   get borderGreyScaleOpt (): Prim.Option<Int> {
      return this._borderGreyScaleOpt.val
   }

   get child (): View {
      return this._child.val
   }
}

export class Word extends View {
   _bold: ITraced<Bool>
   _greyScale: ITraced<Int>
   _str: ITraced<Str>

   static at (α: Addr, bold: ITraced<Bool>, greyScale: ITraced<Int>, str: ITraced<Str>): Word {
      const this_: Word = create(α, Word)
      this_._bold = typeCheck_(bold, Bool)
      this_._greyScale = greyScale
      this_._str = str
      this_.__version()
      return this_
   }

   __visit<T> (v: ViewVisitor<T>): T {
      return v.is_Word(this)
   }

   get bold (): Bool {
      return this._bold.val
   }

   get greyScale (): Int {
      return this._greyScale.val
   }

   get str(): Str {
      return this._str.val
   }
}

export class Vert extends View {
   _child1: ITraced<View>
   _child2: ITraced<View>

   static at (α: Addr, child1: ITraced<View>, child2: ITraced<View>): Vert {
      const this_: Vert = create(α, Vert)
      this_._child1 = typeCheck_(child1, View)
      this_._child2 = typeCheck_(child2, View)
      this_.__version()
      return this_
   }

   __visit<T> (v: ViewVisitor<T>): T {
      return v.is_Vert(this)
   }

   get child1 (): View {
      return this._child1.val
   }

   get child2 (): View {
      return this._child2.val
   }
}

export class Horiz extends View {
   _child1: ITraced<View>
   _child2: ITraced<View>

   static at (α: Addr, child1: ITraced<View>, child2: ITraced<View>): Horiz {
      const this_ = create(α, Horiz)
      this_._child1 = typeCheck_(child1, View)
      this_._child2 = typeCheck_(child2, View)
      this_.__version()
      return this_
   }

   __visit<T> (v: ViewVisitor<T>): T {
      return v.is_Horiz(this)
   }

   get child1 (): View {
      return this._child1.val
   }

   get child2 (): View {
      return this._child2.val
   }
}
