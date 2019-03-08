import { classOf } from "./util/Core"
import { Lattice } from "./util/Ord"
import { Persistent, PersistentClass, PersistentObject, Versioned, asVersioned, at, fieldVals } from "./util/Persistent"

export class BoolLattice implements Lattice<boolean> {
   bot = false
   top = true

   join (...bs: boolean[]): boolean {
      return bs.reduce((b1, b2) => b1 || b2)
   }

   meet (...bs: boolean[]): boolean {
      return bs.reduce((b1, b2) => b1 && b2)
   }
}

export const ann: Lattice<Annotation> = new BoolLattice()
export type Annotation = boolean // for now

export abstract class Annotated implements PersistentObject {
   α: Annotation

   abstract constructor_ (...args: Persistent[]): void // annoying to have to dup method signature

   // Could avoid these shenanigans if we had AnnotatedValue as an explicit wrapper.
   copyAt<T extends Annotated & PersistentObject> (k: PersistentObject, α: Annotation): T {
      const cls: PersistentClass<T> = classOf(this) as PersistentClass<Annotated & PersistentObject> as PersistentClass<T> // TS can't cope
      return at<PersistentObject, T>(k, cls, α, ...fieldVals(this).slice(1))
   }

   setα (α: Annotation): void {
      const hereʹ: Versioned<this> = asVersioned(this)
      hereʹ.copyAt(hereʹ.__id, α)
   }
}
