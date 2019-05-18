import { Annotation, ann } from "./util/Annotated2"
import { className, error } from "./util/Core"
import { Constr } from "./DataType2"
import { Kont } from "./Expr2"
import { Env } from "./Env2"
import { Value, _ } from "./Value2"
import { getα } from "./Versioned2"

// Func to distinguish from expression-level Fun. See GitHub issue #128.
export abstract class Func<K> extends Value<"Func"> {
   abstract __apply (v: Value): [Env, K, Annotation]
}

// Concrete instances must have a field per constructor, in *lexicographical* order.
export abstract class ConstrFunc<K extends Kont<K>> extends Func<K> {
   __apply (v: Value): [Env, K, Annotation] {
      if (v instanceof Constr) {
         const [ρ, κ, α]: [Env, K, Annotation] = ((this as any)[className(v)] as ArgsFunc<K>).__apply(v.fieldValues())
         return [ρ, κ, ann.meet(getα(v), α)]
      } else {
         return error(`Pattern mismatch: ${className(v)} is not a data type.`, v, this)
      }
   }
}

export abstract class ArgsFunc<K> extends Value<"ArgsFunc"> {
   abstract __apply (v̅: Value[]): [Env, K, Annotation]
}
