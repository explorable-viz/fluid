import { as, className, error } from "./util/Core"
import { Constr } from "./DataType2"
import { Kont } from "./Expr2"
import { Env } from "./Env2"
import { Value, _ } from "./Value2"

// Func to distinguish from expression-level Fun. See GitHub issue #128.
export abstract class Func<K> extends Value<"Func"> {
   abstract __apply (v: Value): [Env, K]
}

// Concrete instances must have a field per constructor, in *lexicographical* order.
export abstract class ConstrFunc<K extends Kont<K>> extends Func<K> {
   __apply (v: Value): [Env, K] {
      if (v instanceof Constr) {
         return as((this as any)[className(v)], ArgsFunc).__apply(v.fieldValues())
      } else {
         return error(`Pattern mismatch: ${className(v)} is not a data type.`, v, this)
      }
   }
}

export abstract class ArgsFunc<K> extends Value<"ArgsFunc"> {
   abstract __apply (v̅: Value[]): [Env, K]
}
