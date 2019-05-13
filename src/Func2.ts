import { Value } from "./Value2"

// Func to distinguish from expression-level Fun.
export abstract class Func extends Value {
   abstract __apply (v: Value): Value
}
