import * as $runtime from "../runtime.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
const identity = x => x;
const bimap = dict => dict.bimap;
const lmap = dictBifunctor => f => dictBifunctor.bimap(f)(identity);
const rmap = dictBifunctor => dictBifunctor.bimap(identity);
const bifunctorTuple = {bimap: f => g => v => Data$dTuple.$Tuple(f(v._1), g(v._2))};
const bifunctorEither = {
  bimap: v => v1 => v2 => {
    if (v2.tag === "Left") { return Data$dEither.$Either("Left", v(v2._1)); }
    if (v2.tag === "Right") { return Data$dEither.$Either("Right", v1(v2._1)); }
    $runtime.fail();
  }
};
const bifunctorConst = {bimap: f => v => v1 => f(v1)};
export {bifunctorConst, bifunctorEither, bifunctorTuple,  identity,  };
