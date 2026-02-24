import * as $runtime from "../runtime.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dProfunctor from "../Data.Profunctor/index.js";
const identity = x => x;
const right = dict => dict.right;
const left = dict => dict.left;
const splitChoice = dictCategory => {
  const $0 = dictCategory.Semigroupoid0();
  return dictChoice => l => r => $0.compose(dictChoice.right(r))(dictChoice.left(l));
};
const fanin = dictCategory => {
  const identity1 = dictCategory.identity;
  const $0 = dictCategory.Semigroupoid0();
  const $1 = dictCategory.Semigroupoid0();
  return dictChoice => l => r => $0.compose(dictChoice.Profunctor0().dimap(v2 => {
    if (v2.tag === "Left") { return v2._1; }
    if (v2.tag === "Right") { return v2._1; }
    $runtime.fail();
  })(identity)(identity1))($1.compose(dictChoice.right(r))(dictChoice.left(l)));
};
const choiceFn = /* #__PURE__ */ (() => (
  {
    left: v => v1 => {
      if (v1.tag === "Left") { return Data$dEither.$Either("Left", v(v1._1)); }
      if (v1.tag === "Right") { return Data$dEither.$Either("Right", v1._1); }
      $runtime.fail();
    },
    right: Data$dEither.functorEither.map,
    Profunctor0: () => Data$dProfunctor.profunctorFn
  }
))();
export {choiceFn, fanin,    };
