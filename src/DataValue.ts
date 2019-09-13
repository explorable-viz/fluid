import { __nonNull } from "./util/Core"
import { Expl } from "./Expl"
import { DataValueTag, Value, _, make } from "./Value"

// Value of a datatype constructor; children are always user-level values (i.e. not ES6 primitives).
export class DataValue<Tag extends DataValueTag = DataValueTag> extends Value<Tag> {
   child (k: string): Value {
      return super.child(k) as Value
   }

   children (): Value[] {
      return super.children() as Value[]
   }
}

// Here to break cyclic dependency.
export class Expl_<T extends Value = Value> extends DataValue<"Expl_"> {
   t: Expl = _
   v: T = _
}

export function expl<T extends Value = Value> (t: Expl, v: T): Expl_<T> {
   return make(Expl_, t, v) as Expl_<T>
}
