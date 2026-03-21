import * as Control$dMonad$dGen from "../Control.Monad.Gen/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Foreign$dObject from "../Foreign.Object/index.js";
const fromFoldable = /* #__PURE__ */ Foreign$dObject.fromFoldable(Data$dList$dTypes.foldableList);
const genForeignObject = dictMonadRec => dictMonadGen => {
  const Bind1 = dictMonadGen.Monad0().Bind1();
  const Apply0 = Bind1.Apply0();
  const $0 = Apply0.Functor0();
  const unfoldable1 = Control$dMonad$dGen.unfoldable(dictMonadRec)(dictMonadGen)(Data$dList$dTypes.unfoldableList);
  return genKey => genValue => dictMonadGen.sized(size => Bind1.bind(dictMonadGen.chooseInt(0)(size))(newSize => dictMonadGen.resize(v => newSize)($0.map(fromFoldable)(unfoldable1(Apply0.apply($0.map(Data$dTuple.Tuple)(genKey))(genValue))))));
};
export {fromFoldable, genForeignObject};
