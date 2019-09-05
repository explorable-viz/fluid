import { zipWith } from "./util/Array"
import { __nonNull } from "./util/Core"
import { Annotated } from "./Annotated"
import { Expl } from "./Expl"
import { DataValueTag, State, Value, _, fields, make } from "./Value"

// Value of a datatype constructor; fields are always user-level values (i.e. not ES6 primitives).
export class DataValue<Tag extends DataValueTag = DataValueTag> extends Value<Tag> {
   // what ensures that this is always defined?
   __expl: DataExpl

   children (): Value[] {
      return fields(this).map(k => (this as any as State)[k] as Value)
   }

   explChildren(): Expl_[] {
      return zipWith(expl)(__nonNull(this.__expl).children(), this.children() as Annotated<Value>[])
   }
}

export class DataExpl extends DataValue<"DataExpl"> {
   children (): Expl[] {
      return fields(this).map(k => (this as any as State)[k] as Expl)
   }
}

// Here to break cyclic dependency.
export class Expl_<T extends Value = Value> extends DataValue<"Expl_"> {
   t: Expl = _
   v: Annotated<T> = _
}

export function expl<T extends Value = Value> (t: Expl, v: Annotated<T>): Expl_<T> {
   return make(Expl_, t, v) as Expl_<T>
}
