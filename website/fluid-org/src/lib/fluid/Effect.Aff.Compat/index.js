// | This module provides compatability functions for constructing `Aff`s which
// | are defined via the FFI.
import * as Data$dEither from "../Data.Either/index.js";
import * as Effect$dAff from "../Effect.Aff/index.js";
const EffectFnCanceler = x => x;
const EffectFnAff = x => x;
const fromEffectFnAff = v => Effect$dAff.makeAff(k => () => {
  const v1 = v(x => k(Data$dEither.$Either("Left", x))(), x => k(Data$dEither.$Either("Right", x))());
  return e => Effect$dAff.makeAff(k2 => () => {
    v1(e, x => k2(Data$dEither.$Either("Left", x))(), x => k2(Data$dEither.$Either("Right", x))());
    return Effect$dAff.nonCanceler;
  });
});
export {  fromEffectFnAff};
