import { AClass, Class, __nonNull, assert } from "./util/Core"
import { DataExpl, DataValue } from "./DataValue"
import { DataElim } from "./Match"
import { Str, _, fields } from "./Value"

// Neither of these is currently reflective because of non-standard fields.
export class DataType {
   name: string
   elimC: Class<DataElim>            
   ctrs: Map<string, Ctr>                 // fields of my constructors
   explC̅: Map<string, Class<DataExpl>>    // "explanation" class per constructor

   constructor (
      name: string, 
      elimC: Class<DataElim>, 
      ctrs: Map<string, Ctr>, 
      explC̅: Map<string, Class<DataExpl>>
   ) {
      this.name = name
      this.elimC = elimC
      this.ctrs = ctrs
      this.explC̅ = explC̅
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

// Populated by initDataTypes(). Constructors are not yet first-class.
export let ctrToDataType: Map<string, DataType> = new Map
export const elimSuffix: string = "Elim"
export const explSuffix: string = "Expl"

// See https://stackoverflow.com/questions/33605775 for the dynamic class-naming idiom.
export function initDataType<T extends DataValue> (D: AClass<T>, C̅: Class<T>[]) {
   C̅.sort((C, Cʹ): number => C.name.localeCompare(Cʹ.name)) // probably consistent with string <
   const ctrs: [string, Ctr][] = C̅.map(
            (C: Class<T>): [string, Ctr] => [C.name, new Ctr(C, fields(new C))]
         ),
         elimC_name: string = D.name + elimSuffix,
         elimC: Class<DataElim> = {
            [elimC_name]: class extends DataElim {
               constructor () {
                  super()
                  // lexicographical order hopefully preserved by getOwnPropertyNames()
                  C̅.forEach((C: Class<T>): void => {
                     (this as any)[C.name] = _
                  })
               }
            }
         }[elimC_name],
         explC_name: string = D.name + explSuffix,
         explC̅: [string, Class<DataExpl>][] = ctrs.map(([cʹ, c]: [string, Ctr]) => {
            return [cʹ, {
               [explC_name]: class extends DataExpl {
                  constructor () {
                     super()
                     c.f̅.forEach((f: string): void => {
                        (this as any)[f] = _
                     })
                  }
               }
            }[explC_name]]
         }),
         d: DataType = new DataType(D.name, elimC, new Map(ctrs), new Map(explC̅))
   C̅.forEach((C: Class<T>): void => {
      ctrToDataType.set(C.name, d)
   })
}
