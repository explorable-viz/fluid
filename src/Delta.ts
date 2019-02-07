import { Env } from "./Env"
import { ObjectState, VersionedObject } from "./Runtime"
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

// Generic implementation. No implements clause because I don't statically specify my members.
export class DeltaVersionedObject<T> {
   constructor_ (...args: Object[]): void {
      // TODO: set properties
   }
}

export function diffClosure (tgt: Value.Closure, src: Value.Closure): Delta<Value.Closure, "__subtag" | keyof VersionedObject> {
   const ρ: DeltaRef<Env> = new DeltaRef<Env>(RefDelta.Unchanged, tgt.ρ)
   const σ: DeltaRef<Trie<Traced>> = new DeltaRef<Trie<Traced>>(RefDelta.Changed, src.σ)
   return {
      ρ: ρ,
      σ: σ
   }
}

export function diff<T extends VersionedObject> (tgt: T, src: T): Delta<T, keyof VersionedObject> {
   const tgt_: ObjectState = tgt as Object as ObjectState, // cast a suitable spell
         src_: ObjectState = src as Object as ObjectState
   Object.keys(tgt).map((k: string) => diff(tgt_[k], src_[k]))
}
