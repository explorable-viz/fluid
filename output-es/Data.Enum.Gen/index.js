import * as $runtime from "../runtime.js";
import * as Control$dMonad$dGen from "../Control.Monad.Gen/index.js";
import * as Data$dEnum from "../Data.Enum/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dNonEmpty from "../Data.NonEmpty/index.js";
import * as Data$dUnfoldable1 from "../Data.Unfoldable1/index.js";
const foldable1NonEmpty = /* #__PURE__ */ Data$dNonEmpty.foldable1NonEmpty(Data$dFoldable.foldableArray);
const genBoundedEnum = dictMonadGen => {
  const elements = Control$dMonad$dGen.elements(dictMonadGen)(foldable1NonEmpty);
  return dictBoundedEnum => {
    const Enum1 = dictBoundedEnum.Enum1();
    const Bounded0 = dictBoundedEnum.Bounded0();
    const v = Enum1.succ(Bounded0.bottom);
    if (v.tag === "Just") { return elements(Data$dNonEmpty.$NonEmpty(Bounded0.bottom, Data$dEnum.enumFromTo(Enum1)(Data$dUnfoldable1.unfoldable1Array)(v._1)(Bounded0.top))); }
    if (v.tag === "Nothing") { return dictMonadGen.Monad0().Applicative0().pure(Bounded0.bottom); }
    $runtime.fail();
  };
};
export {foldable1NonEmpty, genBoundedEnum};
