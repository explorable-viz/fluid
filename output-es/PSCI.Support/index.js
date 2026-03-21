// | This module provides support for the
// | PureScript interactive mode, PSCI.
import * as Effect$dConsole from "../Effect.Console/index.js";
const evalShow = dictShow => ({eval: Effect$dConsole.logShow(dictShow)});
const evalEffectUnit = {eval: x => x};
const $$eval = dict => dict.eval;
const evalEffect = dictEval => (
  {
    eval: x => () => {
      const $0 = x();
      return dictEval.eval($0)();
    }
  }
);
export {$$eval as eval, evalEffect, evalEffectUnit, evalShow};
