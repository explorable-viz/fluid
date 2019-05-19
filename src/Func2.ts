import { Annotation, ann } from "./util/Annotated2"
import { className, error } from "./util/Core"
import { DataValue } from "./DataType2"
import { Kont } from "./Expr2"
import { Env } from "./Env2"
import { Value, _ } from "./Value2"
import { Versioned, asVersioned } from "./Versioned2"

// Func to distinguish from expression-level Fun. See GitHub issue #128.
export abstract class Func<K> extends Value<"Func"> {
   abstract __apply (v: Versioned<Value>): [Env, K, Annotation]
}

// Concrete instances must have a field per constructor, in *lexicographical* order.
export abstract class DataFunc<K extends Kont<K>> extends Func<K> {
   __apply (v: Versioned<Value>): [Env, K, Annotation] {
      if (v instanceof DataValue) {
         const args_f: ArgsFunc<K> = ((this as any)[className(v)] as ArgsFunc<K>),
               v̅: Versioned<Value>[] = (v as DataValue).fieldValues().map(v => asVersioned(v)),
               [ρ, κ, α]: [Env, K, Annotation] = args_f.__apply(v̅)
         return [ρ, κ, ann.meet(v.__α, α)]
      } else {
         return error(`Pattern mismatch: ${className(v)} is not a data type.`, v, this)
      }
   }
}

export abstract class ArgsFunc<K> extends Value<"ArgsFunc"> {
   abstract __apply (v̅: Versioned<Value>[]): [Env, K, Annotation]
}
