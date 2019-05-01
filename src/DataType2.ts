import { Class, __nonNull, funName } from "./util/Core"
import { Cons, Nil } from "./BaseTypes2"
import { Expl, Value } from "./ExplVal2"

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

// TODO: exclude metadata in a way that's consistent with Metadata<T>
export function isField (prop: string): boolean {
   throw new Error
}

// Utterly dependent on fields being provided in declaration order, although not part of spec :-/
export function fields (cls: Class<Value>): string[] {
   const proto: Object = Object.getPrototypeOf(new cls)
   return Object.getOwnPropertyNames(proto).filter(isField)
}
