import { Lattice } from "./util/Ord"
import { PersistentObject, make } from "./util/Persistent"

class Bool implements Lattice<Bool>, PersistentObject {
   b: boolean

   static tt: Bool = Bool.make(true)
   static ff: Bool = Bool.make(false)

   bot (): Bool {
      return Bool.ff
   }

   join (that: Bool): Bool {
      return Bool.make(this.b || that.b)
   }

   top (): Bool {
      return Bool.tt
   }

   meet (that: Bool): Bool {
      return Bool.make(this.b && that.b)
   }

   constructor_ (b: boolean) {
      this.b = b
   }

   static make (b: boolean): Bool {
      return make(Bool, b)
   }
}

type Annotation = Bool

export class Annotated {
   ann: Annotation
}
