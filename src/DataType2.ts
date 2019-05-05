import { AClass, Class, __nonNull, assert, funName } from "./util/Core"
import { Bool, Cons, Empty, False, List, NonEmpty, Nil, Pair, Tree, True } from "./BaseTypes2"
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

// Constructor of a datatype, not to be confused with an instance of such a thing (Constr) or name of such a thing
// (Lex.Ctr). Fields have a total ordering given by the order of definition in the corresponding class.
export class Ctr {
   C: Class<Constr<Value>>
   f̅: string[]

   constructor (C: Class<Constr<Value>>, f̅: string[]) {
      this.C = C
      this.f̅ = f̅
   }

   get name (): string {
      return funName(this.C)
   }
}

export function ctrFor (ctr: string): Ctr {
   return ctrToDataType.get(ctr)!.ctrs.get(ctr)!
}

export function arity (ctr: string): number {
   assert(ctrToDataType.has(ctr), "No such constructor.", ctr)
   return ctrFor(ctr).f̅.length
}

// Exclude metadata according to our convention.
export function isField (prop: string): boolean {
   return !prop.startsWith("__")
}

function fields (v: Constr<Value>): string[] {
   return Object.getOwnPropertyNames(v).filter(isField)
}

export function fieldValues (v: Constr<Value>): Value[] {
   return fields(v).map(k => (v as State)[k])
}

// Populated by initDataTypes(). Constructors are not yet first-class. TODO: reinstate projections.
export let ctrToDataType: Map<string, DataType> = new Map

export function initDataType<T> (D: AClass<T>, ctrC̅: Class<T>[]) {
   const ctrs: [string, Ctr][] = ctrC̅.map(
            (C: Class<T>): [string, Ctr] => [funName(C), new Ctr(C, fields(new C))]
         ),
         datatype: DataType = new DataType(funName(D), new Map(ctrs))
   ctrC̅.forEach((C: Class<T>): void => {
      ctrToDataType.set(funName(C), datatype)
   })
}

// This until we have datatype definitions.
export function initDataTypes (): void {
   initDataType(Bool, [True, False])
   initDataType(List, [Nil, Cons])
   initDataType(Pair, [Pair])
   initDataType(Tree, [Empty, NonEmpty])
}
