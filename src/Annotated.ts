import { __nonNull, classOf, } from "./util/Core"
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

   // Could avoid these shenanigans if we had AnnotatedValue as an explicit wrapper.
   copyAt<T extends Annotated & PersistentObject> (k: PersistentObject, α: Annotation): T {
      const cls: PersistentClass<T> = classOf(this) as PersistentClass<Annotated & PersistentObject> as PersistentClass<T> // TS can't cope
      return at<PersistentObject, T>(k, cls, α, ...fieldVals(this).slice(1))
   }

   setα (α: Annotation): void {
      const hereʹ: Versioned<this> = asVersioned(this)
      hereʹ.copyAt(hereʹ.__id, α)
   }

   // An annotation lattice induces a lattice for me.
   join2<T extends Annotated> (o1: T, o2: T): void {
      this.setα(ann.join(o1.α, o2.α))
      const o1ʹ: ObjectState = o1 as Object as ObjectState        // TypeScript gibberish
      const o2ʹ: ObjectState = o2 as Object as ObjectState        // TypeScript gibberish
      fields(this).forEach((k: string): void => {
         const v: Persistent = (this as Object as ObjectState)[k] // TypeScript gibberish
         if (v instanceof Annotated) {
            v.join2(o1ʹ[k] as Annotated, o2ʹ[k] as Annotated)
         }
      })
   }
}
