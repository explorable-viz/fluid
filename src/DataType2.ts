import { AClass, Class, __nonNull, assert, classOf, funName } from "./util/Core"
import { Bool, Cons, False, List, Nil, True } from "./BaseTypes2"
import { Expl } from "./ExplVal2"
import { Constr, State, Value } from "./Value2"

// Neither of these are reflective because of non-standard fields.

export class DataType {
   name: string
   ctrs: Map<string, Ctr>  // fields of my constructors

   constructor (name: string, ctrs: Map<string, Ctr>) {
      this.name = name
      this.ctrs = ctrs
   }
}

// Constructor of a datatype, not to be confused with an instance of such a thing (Constr) or name of such a thing (Lex.Ctr).
// Fields have a total ordering given by the order of definition in the corresponding class.
export class Ctr {
   cls: Class<Constr<Value>>
   fields: string[]

   constructor (cls: Class<Constr<Value>>, fields: string[]) {
      this.cls = cls
      this.fields = fields
   }

   get name (): string {
      return funName(this.cls)
   }
}

// Populated by initDataTypes(). Constructors are not yet first-class. TODO: reinstate projections.
export let ctrToDataType: Map<string, DataType> = new Map

export function ctrFor (ctr: string): Ctr {
   return ctrToDataType.get(ctr)!.ctrs.get(ctr)!
}

export function arity (ctr: string): number {
   assert(ctrToDataType.has(ctr), "No such constructor.", ctr)
   return ctrFor(ctr).fields.length
}

export function initDataType<T> (baseCls: AClass<T>, ctrs: Class<T>[]) {
   const datatype: DataType = new DataType(
      funName(baseCls), new Map(ctrs.map((ctr: Class<T>): [string, Ctr] => [funName(ctr), new Ctr(ctr, fields(ctr))]))
   )
   ctrs.forEach((ctr: Class<T>): void => {
      ctrToDataType.set(funName(ctr), datatype)
   })
}

// This until we have datatype definitions.
export function initDataTypes (): void {
   initDataType(Bool, [True, False])
   initDataType(Expl.Expl, [Expl.Empty])
   initDataType(List, [Nil, Cons])
}

// Exclude metadata according to our convention.
export function isField (prop: string): boolean {
   return !prop.startsWith("__")
}

// Utterly dependent on fields being provided in declaration order, although not part of spec :-/
function fields (cls: Class<Value>): string[] {
   return Object.getOwnPropertyNames(new cls).filter(isField)
}

export function fieldVals (v: Constr<Value>): Value[] {
   return fields(classOf(v)).map(k => (v as State)[k])
}
