import { zipWith } from "./util/Array"
import { Class, __nonNull, as } from "./util/Core"
import { Expl } from "./Expl"
import { DataValueTag, State, Value, _, fields, make } from "./Value"

// Value of a datatype constructor; children are always user-level values (i.e. not ES6 primitives).
export class DataValue<Tag extends DataValueTag = DataValueTag> extends Value<Tag> {
   // what ensures this is always defined?
   __expl: DataExpl

   children (): Value[] {
      return super.children() as Value[]
   }

   explChildren (): Expl_[] {
      return zipWith(expl)(__nonNull(this.__expl).children(), this.children())
   }

   // Could tighten this up from a typing point of view.
   explChild<T extends Value> (prop: keyof this, C: Class<T>): Expl_<T> {
      return expl((this.__expl as any)[prop], as(this[prop], C))
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
   v: T = _
}

export function expl<T extends Value = Value> (t: Expl, v: T): Expl_<T> {
   return make(Expl_, t, v) as Expl_<T>
}
