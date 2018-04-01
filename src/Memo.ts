import { __nonNull, __shallowCopy, __shallowEq, funName, assert } from "./util/Core"

// Strings ok for small, flat keys; for large/nested keys we'll need trees.
export type Addr = string

export class PersistentObject extends Object {
   __history: this[] = []
   __addr: Addr

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

export function addr (o: PersistentObject): Addr {
   return __nonNull(o.__addr)
}

// Require explicit callee (obtaining via IArguments not permitted in strict mode).
export function key (callee: Function, args: IArguments): Addr {
   return funName(callee) + "(" + Array.from(args).map(o => addr(__nonNull(o))).join(",") + ")"
}

export function keyA (callee: Function, ...args: PersistentObject[]): Addr {
   return funName(callee) + "(" + Array.from(args).map(o => addr(__nonNull(o))).join(",") + ")"
}

export function keyP (α: Addr, ...path: string[]): Addr {
   return α + (path.length === 0 ? "" : ("." + path.join(".")))
}
