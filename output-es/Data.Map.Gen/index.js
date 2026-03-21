import * as Control$dMonad$dGen from "../Control.Monad.Gen/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dMap$dInternal from "../Data.Map.Internal/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
const genMap = dictMonadRec => dictMonadGen => {
  const Bind1 = dictMonadGen.Monad0().Bind1();
  const Apply0 = Bind1.Apply0();
  const $0 = Apply0.Functor0();
  const unfoldable1 = Control$dMonad$dGen.unfoldable(dictMonadRec)(dictMonadGen)(Data$dList$dTypes.unfoldableList);
  return dictOrd => {
    const fromFoldable = Data$dMap$dInternal.fromFoldable(dictOrd)(Data$dList$dTypes.foldableList);
    return genKey => genValue => dictMonadGen.sized(size => Bind1.bind(dictMonadGen.chooseInt(0)(size))(newSize => dictMonadGen.resize(v => newSize)($0.map(fromFoldable)(unfoldable1(Apply0.apply($0.map(Data$dTuple.Tuple)(genKey))(genValue))))));
  };
};
export {genMap};
