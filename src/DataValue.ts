import { __nonNull } from "./util/Core"
import { Expl } from "./Expl"
import { DataValueTag, Value, _, make } from "./Value"

// Value of a datatype constructor; children are always user-level values (i.e. not ES6 primitives).
export class DataValue<Tag extends DataValueTag = DataValueTag> extends Value<Tag> {
   __child (k: string): Value {
      return super.__child(k) as Value
   }

   get __children (): Value[] {
      return super.__children as Value[]
   }
}

// Here to break cyclic dependency.
export class ExplValue<T extends Value = Value> extends DataValue<"ExplValue"> {
   t: Expl = _
   v: T = _
}

export function explValue<T extends Value = Value> (t: Expl, v: T): ExplValue<T> {
   return make(ExplValue, t, v) as ExplValue<T>
}
