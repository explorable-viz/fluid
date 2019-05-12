import { Value, _, make } from "./Value2"

export namespace Expl {
   export abstract class Expl extends Value {
   }

   export class Empty extends Expl {
   }

   export function empty (): Empty {
      return make(Empty)
   }
}

type Expl = Expl.Expl

export type ExplVal = [Expl, Value]
