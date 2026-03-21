import * as $runtime from "../runtime.js";
import * as Effect$dAff from "../Effect.Aff/index.js";
import {describe, itAsync, itPending} from "./foreign.js";
const executeTest = v => {
  const $0 = v._2;
  return itAsync(true)(v._1)(onSuccess => onError => {
    const $1 = Effect$dAff.runAff(v2 => {
      if (v2.tag === "Left") { return onError(v2._1); }
      if (v2.tag === "Right") { return onSuccess; }
      $runtime.fail();
    })($0);
    return () => {$1();};
  });
};
const run = a => () => {
  for (const $0 of a) {
    executeTest($0)();
  }
};
export {executeTest, run};
export * from "./foreign.js";
