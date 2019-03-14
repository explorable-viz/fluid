import { __nonNull, absurd, classOf, } from "./util/Core"
import { Lattice } from "./util/Ord"
import { 
   ObjectState, Persistent, PersistentClass, PersistentObject, Versioned, asVersioned, at, fieldVals, fields 
} from "./util/Persistent"

abstract class LatticeImpl<T> implements Lattice<T> {
   abstract bot: T
   abstract top: T

   join (...ts: T[]): T {
      return ts.reduce((t1, t2) => this.join2(t1, t2))
   }

   abstract join2 (t1: T, t2: T): T

   meet (...ts: T[]): T {
      return ts.reduce((t1, t2) => this.meet2(t1, t2))
   }

   abstract meet2 (t1: T, t2: T): T
}

export class BoolLattice extends LatticeImpl<boolean> {
   bot = false
   top = true

   // Important to assert that arguments are defined since undefined propagates in an unhelpful way.
   join2 (b1: boolean, b2: boolean): boolean {
      return __nonNull(b1) || __nonNull(b2)
   }

   meet2 (b1: boolean, b2: boolean): boolean {
      return __nonNull(b1) && __nonNull(b2)
   }
}

export const ann: Lattice<Annotation> = new BoolLattice()
export type Annotation = boolean // for now

export abstract class Annotated implements PersistentObject {
   α: Annotation

   abstract constructor_ (...args: Persistent[]): void // annoying to have to dup method signature

   // Could avoid these shenanigans if we had AnnotatedValue as an explicit wrapper (depends on α being first argument).
   copyAt<T extends Annotated & PersistentObject> (k: PersistentObject, α: Annotation): T {
      const cls: PersistentClass<T> = classOf(this) as PersistentClass<Annotated & PersistentObject> as PersistentClass<T> // TS can't cope
      return at<PersistentObject, T>(k, cls, α, ...fieldVals(this).slice(1))
   }

   setα (α: Annotation): this {
      const hereʹ: Versioned<this> = asVersioned(this)
      hereʹ.copyAt(hereʹ.__id, α)
      return this
   }
}

// An annotation lattice induces a lattice for any object that potentially contains annotations. They behave with imperative 
// LVar-like semantics, so although there is a notion of join/meet, we don't actually need to define them.
export function setall<T extends Persistent> (tgt: T, α: Annotation): T {
   if (tgt === null || typeof tgt === "number" || typeof tgt === "string") {
      return tgt
   } else
   if (tgt instanceof Object) { // annoying that PersistentObject isn't a class
      if (tgt instanceof Annotated) {
         tgt.setα(α)
      }
      fields(tgt).forEach((k: string): void => {
         if (k !== "α") { // perhaps α shouldn't be an enumerable field 
            setall((tgt as Object as ObjectState)[k], α) // TypeScript gibberish
         }
      })
      return tgt
   } else {
      return absurd()
   }
}
