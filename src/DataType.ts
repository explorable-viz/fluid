import { AClass, Class, __nonNull, assert } from "./util/Core"
import { DataValue } from "./DataValue"
import { Expl } from "./Expl"
import { DataElim } from "./Match"
import { Num, PrimValue, Str, _, fields } from "./Value"
import { ν, str_ } from "./Versioned"

export class PrimType {
   name: Str
   C: Class<PrimValue>

   constructor (name: Str, C: Class<PrimValue>) {
      this.name = name
      this.C = C
   }
}

// Neither of these is currently reflective because of non-standard fields.
export class DataType {
   name: Str
   elimC: Class<DataElim>            
   ctrs: Map<string, Ctr>                    // fields of my constructors
   explC̅: Map<string, Class<Expl.DataExpl>>  // "explanation" class per constructor

   constructor (
      name: Str,
      elimC: Class<DataElim>, 
      ctrs: Map<string, Ctr>, 
      explC̅: Map<string, Class<Expl.DataExpl>>
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
export const types: Map<string, DataType | PrimType> = new Map
export const ctrToDataType: Map<string, DataType> = new Map
export const elimToDataType: Map<string, DataType> = new Map
export const elimSuffix: string = "Elim"
export const explSuffix: string = "Expl"

// See https://stackoverflow.com/questions/33605775 for the dynamic class-naming idiom.
export function initDataType<T extends DataValue> (D: AClass<T>, C̅: Class<T>[]) {
   C̅.sort((C, Cʹ): number => C.name.localeCompare(Cʹ.name)) // consistent with Str.leq
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
         explC̅: [string, Class<Expl.DataExpl>][] = ctrs.map(([cʹ, c]: [string, Ctr]) => {
            return [cʹ, {
               [explC_name]: class extends Expl.DataExpl {
                  constructor () {
                     super()
                     c.f̅.forEach((f: string): void => {
                        (this as any)[f] = _
                     })
                  }
               }
            }[explC_name]]
         }),
         d: DataType = new DataType(str_(D.name)(ν()), elimC, new Map(ctrs), new Map(explC̅))
   C̅.forEach((C: Class<T>): void => {
      ctrToDataType.set(C.name, d)
   })
   elimToDataType.set(elimC_name, d)
   types.set(d.name.val, d)
}

types.set(Num.name, new PrimType(str_(Num.name)(ν()), Num))
types.set(Str.name, new PrimType(str_(Str.name)(ν()), Str))
