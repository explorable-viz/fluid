import { Class } from "./util/Core"

// TODO: doh, this is only a constructor of a datatype, not a datatype.
export type DataType = {
   cls: Class<Value>, 
   fields: string[]
} 
