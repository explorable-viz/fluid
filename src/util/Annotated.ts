import { __nonNull } from "./Core"
import { BooleanLattice } from "./Ord"

// Actually a boolean lattice...
abstract class LatticeImpl<T> implements BooleanLattice<T> {
   abstract bot: T
   abstract top: T

   join (...t̅: T[]): T {
      return t̅.reduce((t1, t2) => this.join2(t1, t2))
   }

   meet (...t̅: T[]): T {
      return t̅.reduce((t1, t2) => this.meet2(t1, t2))
   }

   abstract join2 (t1: T, t2: T): T
   abstract meet2 (t1: T, t2: T): T
   abstract negate (t: T): T
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

   negate (b: boolean): boolean {
      return !b
   }
}

export const ann: BooleanLattice<Annotation> = new BoolLattice()
export type Annotation = boolean // for now
