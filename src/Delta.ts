import { Env } from "./Env"
import { World } from "./Runtime"
import { Traced, Value } from "./Traced"
import { PersistentObject } from "./util/Core"

type Trie<T> = Traced.Trie<T>

// https://stackoverflow.com/questions/48215950
type Omit<T, K extends keyof T> = Pick<T, Exclude<keyof T, K>>

// EXPERIMENT
export type Delta<T, K extends keyof T> = {
   [P in keyof Omit<T, K>]: T[P] extends PersistentObject ? DeltaRef<T[P]> : T[P] 
}

enum RefDelta {
   Unchanged,
   Changed
}

class DeltaRef<T extends Object> {
   constructor (
      public delta: RefDelta,
      public ref: T
   ) {
   }
}

type OmittedProps = "__id" | "__commit" | "__mostRecent" | "__tag" | "__subtag" | "eq" | "constructor_"

export function diff (tgt: Value.Closure, src: Value.Closure): Delta<Value.Closure, OmittedProps> {
   const ρ: DeltaRef<Env> = new DeltaRef<Env>(RefDelta.Unchanged, tgt.ρ)
   const σ: DeltaRef<Trie<Traced>> = new DeltaRef<Trie<Traced>>(RefDelta.Changed, src.σ)
   return {
      ρ: ρ,
      σ: σ
   }
}
