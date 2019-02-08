import { VersionedObject, World } from "./Runtime"

// Generic implementation. No implements clause because I don't statically specify my members.
export class DeltaVersionedObject<T> {
   constructor_ (...args: Object[]): void {
      // TODO: set properties
   }
}

// For now just assume changed/not changed.
export function diff<T extends VersionedObject> (o: T, k: keyof T, w: World): boolean {
   
}

/*
export function diff<T extends Object> (tgt: T | null, src: T | null): DeltaRef<T> {
   if (tgt instanceof ValueObject && src instanceof ValueObject) {
      return new DeltaRef(tgt.eq(src), tgt)
   } else {
      return new DeltaRef(tgt === src, tgt)
   }
}
*/
