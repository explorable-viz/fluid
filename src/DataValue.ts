import { zip } from "./util/Array"
import { Annotated } from "./Annotated"
import { Expl } from "./ExplValue"
import { DataValueTag, State, Value, fields } from "./Value"

// Value of a datatype constructor; fields are always user-level values (i.e. not ES6 primitives).
export class DataValue<Tag extends DataValueTag = DataValueTag> extends Value<Tag> {
   __expl: DataExpl

   fieldValues (): Value[] {
      return fields(this).map(k => (this as any as State)[k] as Value)
   }

   fieldExplValues(): [Expl, Annotated<Value>][] {
      return zip(this.__expl.fieldValues(), this.fieldValues() as Annotated<Value>[])
   }
}

export class DataExpl extends DataValue<"DataExpl"> {
   fieldValues (): Expl[] {
      return fields(this).map(k => (this as any as State)[k] as Expl)
   }
}
