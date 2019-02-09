import { ValueObject } from "./Persistent"
import { VersionedObject, World, getProp } from "./Versioned"

export function diffProp<T extends VersionedObject> (o: T, k: keyof T, w: World): boolean {
   const v: Object | null = getProp(o, k),
         vʹ: Object | null = getProp(o, k)
   if (vʹ instanceof ValueObject && v instanceof ValueObject) {
      return vʹ.eq(v)
   } else {
      return vʹ === v
   }
}
