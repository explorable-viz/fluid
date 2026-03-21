import * as $runtime from "../runtime.js";
import * as Control$dMonad$dGen from "../Control.Monad.Gen/index.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dIdentity from "../Data.Identity/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNonEmpty from "../Data.NonEmpty/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
const max = x => y => {
  const v = Data$dOrd.ordInt.compare(x)(y);
  if (v === "LT") { return y; }
  if (v === "EQ") { return x; }
  if (v === "GT") { return x; }
  $runtime.fail();
};
const genTuple = dictApply => a => b => dictApply.apply(dictApply.Functor0().map(Data$dTuple.Tuple)(a))(b);
const genNonEmpty = dictMonadRec => dictMonadGen => {
  const Apply0 = dictMonadGen.Monad0().Bind1().Apply0();
  const unfoldable1 = Control$dMonad$dGen.unfoldable(dictMonadRec)(dictMonadGen);
  return dictUnfoldable => {
    const unfoldable2 = unfoldable1(dictUnfoldable);
    return gen => Apply0.apply(Apply0.Functor0().map(Data$dNonEmpty.NonEmpty)(gen))(dictMonadGen.resize(x => max(0)(x - 1 | 0))(unfoldable2(gen)));
  };
};
const genMaybe$p = dictMonadGen => {
  const Monad0 = dictMonadGen.Monad0();
  const Bind1 = Monad0.Bind1();
  return bias => gen => Bind1.bind(dictMonadGen.chooseFloat(0.0)(1.0))(n => {
    if (n < bias) { return Bind1.Apply0().Functor0().map(Data$dMaybe.Just)(gen); }
    return Monad0.Applicative0().pure(Data$dMaybe.Nothing);
  });
};
const genMaybe = dictMonadGen => genMaybe$p(dictMonadGen)(0.75);
const genIdentity = dictFunctor => dictFunctor.map(Data$dIdentity.Identity);
const genEither$p = dictMonadGen => {
  const Bind1 = dictMonadGen.Monad0().Bind1();
  const $0 = Bind1.Apply0().Functor0();
  return bias => genA => genB => Bind1.bind(dictMonadGen.chooseFloat(0.0)(1.0))(n => {
    if (n < bias) { return $0.map(Data$dEither.Left)(genA); }
    return $0.map(Data$dEither.Right)(genB);
  });
};
const genEither = dictMonadGen => genEither$p(dictMonadGen)(0.5);
export {genEither, genEither$p, genIdentity, genMaybe, genMaybe$p, genNonEmpty, genTuple, max};
