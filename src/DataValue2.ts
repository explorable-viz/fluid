import { Expl } from "./ExplValue2"
import { DataValueTag, State, Value, fields } from "./Value2"

// Value of a datatype constructor; fields are always user-level values (i.e. not ES6 primitives).
export class DataValue<Tag extends DataValueTag = DataValueTag> extends Value<Tag> {
   __expl: DataExpl

   fieldValues (): Value[] {
      return fields(this).map(k => (this as any as State)[k] as Value)
   }
}

export class DataExpl extends DataValue<"DataExpl"> {
   fieldValues (): Expl[] {
      return fields(this).map(k => (this as any as State)[k] as Expl)
   }
}
