import { assert } from "./util/Core"

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

// Populated by initDataTypes(). Constructors are not yet first-class. TODO: reinstate projections.
export let ctrToDataType: Map<string, DataType> = new Map

export function arity (ctr: string): number {
   assert(ctrToDataType.has(ctr), "No such constructor.", ctr)
   return ctrToDataType.get(ctr)!.ctrs.get(ctr)!.length
}

function initDataType <T> (d: DataType): void {
   d.ctrs.forEach((_, ctr: string): void => {
      ctrToDataType.set(ctr, d)
   })
}

export function initDataTypes (): void {
   assert(ctrToDataType.size === 0)
   initDataType(new DataType("Bool", new Map([["True", []], ["False", []]])))
   initDataType(new DataType("Box", new Map([["Box", ["unbox"]]])))
   initDataType(new DataType("List", new Map([["Nil", []], ["Cons", ["head", "tail"]]])))
   initDataType(new DataType("Ordering", new Map([["LT", []], ["GT", []], ["EQ", []]])))
   initDataType(new DataType("Pair", new Map([["Pair", ["fst", "snd"]]])))
   initDataType(new DataType("Point", new Map([["Point", ["x", "y"]]])))
   initDataType(new DataType("Rect", new Map([["Rect", ["width", "height"]]])))
   initDataType(new DataType("Option", new Map([["None", []], ["Some", ["valOf"]]])))
   initDataType(new DataType("Tree", new Map([["Empty", []], ["NonEmpty", ["left", "t", "right"]]])))
   initDataType(new DataType("Unit", new Map([["Unit", []]])))
   initDataType(new DataType("GraphicsElement", new Map([
      ["PathStroke", ["points"]], 
      ["RectFill", ["points"]], 
      ["Translate", ["vec", "elem"]],
      ["Graphic", ["elems"]]
   ])))
}
