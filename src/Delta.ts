import { assert } from "./util/Core"
import { Persistent, State, Value } from "./Value"

export type Delta = Map<Value, State>

export const __delta: Delta = new Map()

export function setDelta (v: Value, prop: string, u: Persistent) {
   let v_delta: State | undefined = __delta.get(v)
   if (v_delta === undefined) {
      __delta.set(v, { [prop]: u })
   } else {
      if (v_delta[prop] !== undefined) {
         assert(v_delta[prop] === u)
      } else {
         v_delta[prop] = u
         __delta.set(v, v_delta)
      }
   }
}

export function clearDelta (): void {
   __delta.clear()
}
