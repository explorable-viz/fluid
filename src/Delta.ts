import { ValueObject } from "./util/Core"
import { VersionedObject, World, getProp } from "./Runtime"

export function diffProp<T extends VersionedObject> (o: T, k: keyof T, w: World): boolean {
   const v: Object | null = getProp(o, k),
         vʹ: Object | null = getProp(o, k)
   if (vʹ instanceof ValueObject && v instanceof ValueObject) {
      return vʹ.eq(v)
   } else {
      return vʹ === v
   }
}
