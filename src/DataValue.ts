import { zipWith } from "./util/Array"
import { __nonNull, absurd, assert } from "./util/Core"
import { Expl } from "./Expl"
import { DataValueTag, Value, _, make } from "./Value"

// Value of a datatype constructor; children are always user-level values (i.e. not ES6 primitives).
export class DataValue<Tag extends DataValueTag = DataValueTag> extends Value<Tag> {
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

// TODO: UnaryApp, BinaryApp need some work here.
export function explChildren (t: Expl, v: DataValue): Expl_[] {
   if (t instanceof Expl.DataExpl) {
      return zipWith(expl)(t.children(), v.children())
   } else 
   // Perhaps syntactically unify all these "tail-call" forms?
   if (t instanceof Expl.Defs) {
      assert(v === t.tv.v)
      return explChildren(t.tv.t, v)
   } else
   if (t instanceof Expl.MatchAs) {
      assert(v === t.tv.v)
      return explChildren(t.tv.t, v)
   } else
   if (t instanceof Expl.Typematch) {
      assert(v === t.tv.v)
      return explChildren(t.tv.t, v)
   } else
   if (t instanceof Expl.Var) {
      assert(v === t.tv.v)
      return explChildren(t.tv.t, v)
   } else {
      return absurd()
   }
}
