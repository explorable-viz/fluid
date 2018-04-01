import { __nonNull, assert, funName } from "./util/Core"

export function addr (o: Object): Addr {
   return __nonNull(o.__addr)
}

// Require explicit callee (obtaining via IArguments not permitted in strict mode).
export function key (callee: Function, args: IArguments): Addr {
   return funName(callee) + "(" + Array.from(args).map(o => addr(__nonNull(o))).join(",") + ")"
}

export function keyA (callee: Function, ...args: Object[]): Addr {
   return addr(callee) + "(" + Array.from(args).map(o => addr(__nonNull(o))).join(",") + ")"
}

export function keyP (α: Addr, ...path: string[]): Addr {
   return α + (path.length === 0 ? "" : ("." + path.join(".")))
}
