import { VersionedObject, World, getProp } from "./Persistent"

export function diffProp<T extends VersionedObject> (o: T, k: keyof T, w: World): boolean {
   return getProp(o, k) === getProp(o, k) // TODO: use w
}
