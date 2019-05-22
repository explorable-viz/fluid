import { Annotation, ann } from "./util/Annotated2"
import { assert, className, error } from "./util/Core"
import { DataValue, elimNameSuffix } from "./DataType2"
import { Kont } from "./Expr2"
import { Env } from "./Env2"
import { Value, _, make } from "./Value2"
import { Versioned, asVersioned } from "./Versioned2"

export class Plug<K extends Kont<K>, M extends Match<K>> extends DataValue<"Plug"> {
   ξ: M = _
   κ: K = _ // fills the single hole in ξ
}

export function plug<K extends Kont<K>, M extends Match<K>> (ξ: M, κ: K): Plug<K, M> {
   return make(Plug, ξ, κ) as Plug<K, M>
}

export class ArgsPlug<K extends Kont<K>, M extends ArgsMatch<K>> extends DataValue<"ArgsPlug"> {
   Ψ: M = _
   κ: K = _ // fills the single hole in Ψ
}

export function argsPlug<K extends Kont<K>, M extends ArgsMatch<K>> (ξ: M, κ: K): ArgsPlug<K, M> {
   return make(ArgsPlug, ξ, κ) as ArgsPlug<K, M>
}

// Func to distinguish from expression-level Fun. See GitHub issue #128.
export abstract class Func<K extends Kont<K>> extends Value<"Func"> {
   abstract __apply (v: Versioned<Value>): [Env, Plug<K, Match<K>>, Annotation]
}

export abstract class Match<K> extends Value<"Match"> {
   abstract __unapply (): void
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
               [ρ, {κ}, α]: [Env, ArgsPlug<K, ArgsMatch<K>>, Annotation] = args_f.__apply(v̅)
         return [ρ, plug(dataMatch(), κ), ann.meet(v.__α, α)]
      } else {
         return error(`Pattern mismatch: ${className(v)} is not a datatype.`, v, this)
      }
   }
}

export class DataMatch<K extends Kont<K>> extends Match<K> {
   __unapply (): void {
   }
}

function dataMatch<K extends Kont<K>> (): DataMatch<K> {
   return make(DataMatch)
}

export abstract class ArgsFunc<K extends Kont<K>> extends Value<"ArgsFunc"> {
   abstract __apply (v̅: Versioned<Value>[]): [Env, ArgsPlug<K, ArgsMatch<K>>, Annotation]
}

export abstract class ArgsMatch<K> extends Value<"ArgsMatch"> {
   abstract __unapply (): void
}
