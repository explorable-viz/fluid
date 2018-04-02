import { __nonNull, __shallowCopy, __shallowEq, funName, assert } from "./util/Core"

export type RawId = number

export class Id {
   __Id() {
      // descriminator
   }
}

export class PersistentObject<T extends Id> extends Object {
   __history: this[] = []
   __id: T

   // At a given version (there is only one, currently) enforce "single assignment" semantics.
   __version (): Object {
      if (this.__history.length === 0) {
         this.__history.push(__shallowCopy(this))
      } else {
         assert(__shallowEq(this, this.__history[0]))
      }
      return this
   }
}

Object.defineProperty(PersistentObject.prototype, "__version", {
   enumerable: false
})
