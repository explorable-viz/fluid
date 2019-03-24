import { __nonNull, classOf, } from "./Core"
import { Lattice } from "./Ord"
import { MemoArgs, MemoFunType, Persistent, PersistentClass, PersistentObject, memo } from "./Persistent"
import { ObjectState, Versioned, asVersioned, at, fieldVals, fields } from "./Versioned"

abstract class LatticeImpl<T> implements Lattice<T> {
   abstract bot: T
   abstract top: T

   join (...t̅: T[]): T {
      return t̅.reduce((t1, t2) => this.join2(t1, t2))
   }

   abstract join2 (t1: T, t2: T): T

   meet (...t̅: T[]): T {
      return t̅.reduce((t1, t2) => this.meet2(t1, t2))
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

   abstract constructor_ (...v̅: MemoArgs): void // annoying to have to dup method signature

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

   joinα (α: Annotation): this {
      const hereʹ: Versioned<this> = asVersioned(this)
      hereʹ.copyAt(hereʹ.__id, ann.join(this.α, α))
      return this
   }
}

// Memoising an imperative function makes any side effects idempotent. Not clear yet how to "partially" memoise LVar-like 
// functions like joinα, but setall isn't one of those.
export function setall<T extends PersistentObject> (tgt: T, α: Annotation): T {
   return memo<T>(setall_ as MemoFunType<T>, tgt, α)
}

// An annotation lattice induces a lattice for any object that potentially contains annotations. They behave with imperative 
// LVar-like semantics, so although there is a notion of join/meet, we don't actually need to define them.
export function setall_<T extends PersistentObject> (tgt: T, α: Annotation): T {
   if (tgt instanceof Annotated) {
      tgt.setα(α)
   }
   fields(tgt).forEach((k: string): void => {
      const v: Persistent = (tgt as Object as ObjectState)[k] // TypeScript gibberish
      if (v instanceof Object) { // annoying that PersistentObject isn't a class
         setall(v as PersistentObject, α) 
      }
   })
   return tgt
}
