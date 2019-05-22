import { Annotation, ann } from "./util/Annotated2"
import { assert, className, error } from "./util/Core"
import { elimNameSuffix } from "./DataType2"
import { Env } from "./Env2"
import { Kont } from "./Expr2"
import { Match } from "./ExplValue2"
import { DataValue, Value, _, make } from "./Value2"
import { Versioned, asVersioned } from "./Versioned2"

import Args = Match.Args
import Plug = Match.Plug
type Match<K> = Match.Match<K>

// Func to distinguish from expression-level Fun. See GitHub issue #128.
export abstract class Func<K extends Kont<K>> extends Value<"Func"> {
   abstract __apply (v: Versioned<Value>): [Env, Plug<K, Match<K>>, Annotation]
}

function datatype (f: DataFunc<any>): string {
   const c: string = className(f)
   return c.substr(0, c.length - elimNameSuffix.length)
}

// Concrete instances must have a field per constructor, in *lexicographical* order.
export abstract class DataFunc<K extends Kont<K>> extends Func<K> {
   __apply (v: Versioned<Value>): [Env, Plug<K, Match<K>>, Annotation] {
      if (v instanceof DataValue) {
         const args_f: ArgsFunc<K> = ((this as any)[className(v)] as ArgsFunc<K>)
         assert(args_f !== undefined, `Pattern mismatch: found ${className(v)}, expected ${datatype(this)}.`)
         const v̅: Versioned<Value>[] = (v as DataValue).fieldValues().map(v => asVersioned(v)),
               [ρ, {κ}, α]: [Env, Args.Plug<K, Args.Match<K>>, Annotation] = args_f.__apply(v̅)
         return [ρ, Match.plug(dataMatch(), κ), ann.meet(v.__α, α)]
      } else {
         return error(`Pattern mismatch: ${className(v)} is not a datatype.`, v, this)
      }
   }
}

export class DataMatch<K extends Kont<K>> extends Match.Match<K> {
   __unapply (): void {
   }
}

function dataMatch<K extends Kont<K>> (): DataMatch<K> {
   return make(DataMatch)
}

export abstract class ArgsFunc<K extends Kont<K>> extends Value<"ArgsFunc"> {
   abstract __apply (v̅: Versioned<Value>[]): [Env, Args.Plug<K, Args.Match<K>>, Annotation]
}
