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
   initDataType(new DataType("Box", new Map([["Box", ["unbox"]]])))
   initDataType(new DataType("List", new Map([["Nil", []], ["Cons", ["head", "tail"]]])))
   initDataType(new DataType("Ordering", new Map([["LT", []], ["GT", []], ["EQ", []]])))
   initDataType(new DataType("Pair", new Map([["Pair", ["fst", "snd"]]])))
   initDataType(new DataType("Option", new Map([["None", []], ["Some", ["valOf"]]])))
   initDataType(new DataType("Tree", new Map([["Empty", []], ["NonEmpty", ["left", "t", "right"]]])))
   initDataType(new DataType("Unit", new Map([["Unit", []]])))
   initDataType(new DataType("View", new Map([
      ["Background", ["greyScale", "child"]],
      ["EmptyView", []],
      ["Word", ["bold", "greyScale", "str"]],
      ["Horiz", ["child1", "child2"]],
      ["RoundedCell", ["borderGreyScaleOpt", "child"]],
      ["Space", []],
      ["Vert", ["child1", "child2"]]
   ])))
}
