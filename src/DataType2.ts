import { AClass, Class, __nonNull, assert } from "./util/Core"
import { DataFunc } from "./Match2"
import { DataValue, State, Str, _, fields } from "./Value2"

// Neither of these is currently reflective because of non-standard fields.
export class DataType {
   name: string
   elimC: Class<DataFunc<any>>   // not sure how better to parameterise 
   ctrs: Map<string, Ctr>        // fields of my constructors

   constructor (name: string, elimC: Class<DataFunc<any>>, ctrs: Map<string, Ctr>) {
      this.name = name
      this.elimC = elimC
      this.ctrs = ctrs
   }
}

// Constructor of a datatype, not to be confused with an instance of such a thing (Constr) or name of such a thing
// (Lex.Ctr). Fields have a total ordering given by the order of definition in the corresponding class.
export class Ctr {
   C: Class<DataValue>
   f̅: string[]

   constructor (C: Class<DataValue>, f̅: string[]) {
      this.C = C
      this.f̅ = f̅
   }
}

export function ctrFor (ctr: Str): Ctr {
   return ctrToDataType.get(ctr.val)!.ctrs.get(ctr.val)!
}

export function arity (ctr: Str): number {
   assert(ctrToDataType.has(ctr.val), `No such constructor: "${ctr.val}".`,)
   return ctrFor(ctr).f̅.length
}

// Populated by initDataTypes(). Constructors are not yet first-class. TODO: reinstate projections.
export let ctrToDataType: Map<string, DataType> = new Map
export const elimNameSuffix: string = "Func"

export function initDataType<T extends DataValue> (D: AClass<T>, ctrC̅: Class<T>[]) {
   ctrC̅.sort((C, Cʹ): number => C.name.localeCompare(Cʹ.name)) // probably consistent with string <
   const ctrs: [string, Ctr][] = ctrC̅.map(
            (C: Class<T>): [string, Ctr] => [C.name, new Ctr(C, fields(new C))]
         ),
         elimC_name: string = D.name + elimNameSuffix,
         elimC: Class<DataFunc<any>> = {
            // https://stackoverflow.com/questions/33605775
            [elimC_name]: class extends DataFunc<any> {
               constructor () {
                  super()
                  // lexicographical order hopefully preserved by getOwnPropertyNames()
                  ctrC̅.forEach((C: Class<T>): void => {
                     (this as any as State)[C.name] = _
                  })
               }
            }
         }[elimC_name],
         datatype: DataType = new DataType(D.name, elimC, new Map(ctrs))
   ctrC̅.forEach((C: Class<T>): void => {
      ctrToDataType.set(C.name, datatype)
   })
}
