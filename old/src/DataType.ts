import { AClass, Class, __nonNull } from "./util/Core"
import { DataValue } from "./DataValue"
import { Expl } from "./Expl"
import { Expr } from "./Expr"
import { DataElim } from "./Match"
import { Num, PrimValue, Str, _, fields } from "./Value"
import { ν, str } from "./Versioned"

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
   exprC̅: Map<string, Class<Expr.DataExpr>>  // "expression" class per constructor
   explC̅: Map<string, Class<Expl.DataExpl>>  // "explanation" class per constructor

   constructor (
      name: Str,
      elimC: Class<DataElim>, 
      ctrs: Map<string, Ctr>, 
      exprC̅: Map<string, Class<Expr.DataExpr>>,
      explC̅: Map<string, Class<Expl.DataExpl>>
   ) {
      this.name = name
      this.elimC = elimC
      this.ctrs = ctrs
      this.exprC̅ = exprC̅
      this.explC̅ = explC̅
   }
}

// Constructor of a datatype, not to be confused with an instance of such a thing (DataValue) or name of such a thing
// (Lex.Ctr). Fields have a total ordering given by the order of definition in the corresponding class.
export class Ctr {
   C: Class<DataValue>
   f̅: string[]

   constructor (C: Class<DataValue>, f̅: string[]) {
      this.C = C
      this.f̅ = f̅
   }

   get arity (): number {
      return this.f̅.length
   }

   get c (): string {
      return this.C.name
   }
}

export function ctrFor (c: string): Ctr {
   return __nonNull(ctrToDataType.get(c), `Unknown constructor ${c}.`).ctrs.get(c)!
}

export function explClass (C: Class<DataValue>): Class<Expl.DataExpl> {
   return __nonNull(ctrToDataType.get(C.name)).explC̅.get(C.name)!
}

export function exprClass (C: Class<DataValue>): Class<Expr.DataExpr> {
   return __nonNull(ctrToDataType.get(C.name)).exprC̅.get(C.name)!
}

export function valueClass (C: Class<Expr.DataExpr>): Class<DataValue> {
   return ctrFor(C.name).C
}

// Populated by initDataTypes(). Constructors are not yet first-class.
export const types: Map<string, DataType | PrimType> = new Map
export const ctrToDataType: Map<string, DataType> = new Map
export const elimToDataType: Map<string, DataType> = new Map

// See https://stackoverflow.com/questions/33605775 for the dynamic class-naming idiom.
export function initDataType<T extends DataValue> (D: AClass<T>, C̅: Class<T>[]) {
   C̅.sort((C, Cʹ): number => C.name.localeCompare(Cʹ.name)) // consistent with Str.leq
   const ctrs: [string, Ctr][] = C̅.map(
            (C: Class<T>): [string, Ctr] => [C.name, new Ctr(C, fields(new C) as string[])]
         ),
         elimC: Class<DataElim> = {
            [D.name]: class extends DataElim {
               constructor () {
                  super()
                  // lexicographical order hopefully preserved by getOwnPropertyNames()
                  C̅.forEach((C: Class<T>): void => {
                     (this as any)[C.name] = _
                  })
               }
            }
         }[D.name],
         exprC̅: [string, Class<Expr.DataExpr>][] = ctrs.map(([c_str, c]: [string, Ctr]) => {
            return [c_str, {
               [c_str]: class extends Expr.DataExpr {
                  constructor () {
                     super()
                     c.f̅.forEach((f: string): void => {
                        (this as any)[f] = _
                     })
                  }
               }
            }[c_str]]
         }),
         explC̅: [string, Class<Expl.DataExpl>][] = ctrs.map(([c_str, c]: [string, Ctr]) => {
            return [c_str, {
               [c_str]: class extends Expl.DataExpl {
                  constructor () {
                     super()
                     c.f̅.forEach((f: string): void => {
                        (this as any)[f] = _
                     })
                  }
               }
            }[c_str]]
         }),
         d: DataType = new DataType(str(D.name)(ν()), elimC, new Map(ctrs), new Map(exprC̅), new Map(explC̅))
   C̅.forEach((C: Class<T>): void => {
      ctrToDataType.set(C.name, d)
   })
   elimToDataType.set(D.name, d)
   types.set(d.name.val, d)
}

types.set(Num.name, new PrimType(str(Num.name)(ν()), Num))
types.set(Str.name, new PrimType(str(Str.name)(ν()), Str))
