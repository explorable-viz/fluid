import { Class, __nonNull, classOf, funName } from "./util/Core"
import { Cons, Nil } from "./BaseTypes2"
import { Expl } from "./ExplVal2"
import { Constr, State, Value } from "./Value2"

// TODO: doh, this is only a constructor of a datatype, not a datatype.
export type DataType = {
   cls: Class<Value>, 
   fields: string[]
} 

// Guess this would be populated at compile-time or by a type provider. Is there a reflective way to access the classes of a module?
const datatypeFor_: Class<Value>[] = 
   [Cons,
    Expl.Empty,
    Nil]
   
export const datatypeFor: Map<string, DataType> = new Map(
      datatypeFor_.map((cls): [string, DataType] => [funName(__nonNull(cls)), { cls, fields: fields(cls) }])
   )

// Exclude metadata in a way that's consistent with the naming convention of Metadata<T>. Ouch.
export function isField (prop: string): boolean {
   return !prop.startsWith("__")
}

// Utterly dependent on fields being provided in declaration order, although not part of spec :-/
export function fields (cls: Class<Value>): string[] {
   return Object.getOwnPropertyNames(new cls).filter(isField)
}

export function fieldVals (v: Constr<Value>): Value[] {
   return fields(classOf(v)).map(k => (v as State)[k])
}
