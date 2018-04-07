import { assert } from "./util/Core"
import { Ctr } from "./Runtime"

// The fields of a constructor have a total ordering independent of their lexicographical ordering.
// This is the order in which they are defined in the class definition (TODO: check). Not interned.
class DataType {
   name: string
   ctrs: Map<string, string[]>  // fields of my constructors

   constructor (name: string, ctrs: Map<string, string[]>) {
      this.name = name
      this.ctrs = ctrs
   }
}

// Populated by initDataTypes(). Note that constructors are not yet first-class. TODO: reinstate projections.
export const constructors: Map<string, Ctr<Object>> = new Map
export let ctrToDataType: Map<string, DataType> = new Map

function initDataType <T> (d: DataType): void {
   d.ctrs.forEach((_, ctr: string): void => {
      ctrToDataType.set(ctr, d)
   })
}
   
export function initDataTypes (): void {
   assert(ctrToDataType.size === 0)
   initDataType(new DataType("Bool", new Map([["Bool", []], ["False", []]])))
}
