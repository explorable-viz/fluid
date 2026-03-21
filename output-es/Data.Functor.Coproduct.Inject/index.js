import * as $runtime from "../runtime.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
const prj = dict => dict.prj;
const injectReflexive = {inj: x => x, prj: Data$dMaybe.Just};
const injectLeft = {
  inj: x => Data$dEither.$Either("Left", x),
  prj: v2 => {
    if (v2.tag === "Left") { return Data$dMaybe.$Maybe("Just", v2._1); }
    if (v2.tag === "Right") { return Data$dMaybe.Nothing; }
    $runtime.fail();
  }
};
const inj = dict => dict.inj;
const injectRight = dictInject => (
  {
    inj: x => Data$dEither.$Either("Right", dictInject.inj(x)),
    prj: v2 => {
      if (v2.tag === "Left") { return Data$dMaybe.Nothing; }
      if (v2.tag === "Right") { return dictInject.prj(v2._1); }
      $runtime.fail();
    }
  }
);
export {inj, injectLeft, injectReflexive, injectRight, prj};
