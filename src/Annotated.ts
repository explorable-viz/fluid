import { Lattice } from "./util/Ord"

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

export class Annotated {
   α: Annotation
}
