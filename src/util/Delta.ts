import { Persistent, World, getProp, versioned } from "./Persistent"

export function diffProp<T extends Persistent> (o: T, k: keyof T, w: World): boolean {
   if (versioned(o)) {
      return getProp(o, k) === getProp(o, k) // TODO: use w
   } else
   if (typeof o === "string" || typeof o === "number") {
      return o === o
   } else {
      return false
   }
}
