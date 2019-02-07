import { Env } from "./Env"
import { Value } from "./Traced"

// EXPERIMENT
export type Delta<T> = {
   [P in keyof T]: DeltaRef<Delta<T[P]>> 
}

enum RefDelta {
   Unchanged,
   Changed
}

class DeltaRef<T extends Object> {
   delta: RefDelta
   ref: T
}

export function blah (v: Delta<Value.Closure>): void {
   const ρ: Delta<Env> = v.ρ.ref
}
