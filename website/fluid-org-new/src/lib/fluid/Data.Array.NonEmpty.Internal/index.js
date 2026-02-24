// | This module exports the `NonEmptyArray` constructor.
// |
// | It is **NOT** intended for public use and is **NOT** versioned.
// |
// | Its content may change **in any way**, **at any time** and
// | **without notice**.
import * as Control$dAlt from "../Control.Alt/index.js";
import * as Control$dApplicative from "../Control.Applicative/index.js";
import * as Control$dApply from "../Control.Apply/index.js";
import * as Control$dBind from "../Control.Bind/index.js";
import * as Control$dMonad from "../Control.Monad/index.js";
import * as Data$dEq from "../Data.Eq/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dFoldableWithIndex from "../Data.FoldableWithIndex/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dFunctorWithIndex from "../Data.FunctorWithIndex/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dSemigroup from "../Data.Semigroup/index.js";
import * as Data$dSemigroup$dTraversable from "../Data.Semigroup.Traversable/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dTraversable from "../Data.Traversable/index.js";
import * as Data$dTraversableWithIndex from "../Data.TraversableWithIndex/index.js";
import * as Data$dUnfoldable1 from "../Data.Unfoldable1/index.js";
import {foldl1Impl, foldr1Impl, traverse1Impl} from "./foreign.js";
const NonEmptyArray = x => x;
const unfoldable1NonEmptyArray = Data$dUnfoldable1.unfoldable1Array;
const traversableWithIndexNonEmptyArray = Data$dTraversableWithIndex.traversableWithIndexArray;
const traversableNonEmptyArray = Data$dTraversable.traversableArray;
const showNonEmptyArray = dictShow => ({show: v => "(NonEmptyArray " + Data$dShow.showArrayImpl(dictShow.show)(v) + ")"});
const semigroupNonEmptyArray = Data$dSemigroup.semigroupArray;
const ordNonEmptyArray = dictOrd => Data$dOrd.ordArray(dictOrd);
const ord1NonEmptyArray = Data$dOrd.ord1Array;
const monadNonEmptyArray = Control$dMonad.monadArray;
const functorWithIndexNonEmptyArray = Data$dFunctorWithIndex.functorWithIndexArray;
const functorNonEmptyArray = Data$dFunctor.functorArray;
const foldableWithIndexNonEmptyArray = Data$dFoldableWithIndex.foldableWithIndexArray;
const foldableNonEmptyArray = Data$dFoldable.foldableArray;
const foldable1NonEmptyArray = {
  foldMap1: dictSemigroup => {
    const append = dictSemigroup.append;
    return f => {
      const $0 = Data$dFunctor.arrayMap(f);
      const $1 = foldable1NonEmptyArray.foldl1(append);
      return x => $1($0(x));
    };
  },
  foldr1: $0 => $1 => foldr1Impl($0, $1),
  foldl1: $0 => $1 => foldl1Impl($0, $1),
  Foldable0: () => Data$dFoldable.foldableArray
};
const traversable1NonEmptyArray = {
  traverse1: dictApply => {
    const apply = dictApply.apply;
    const map = dictApply.Functor0().map;
    return f => traverse1Impl(apply, map, f);
  },
  sequence1: dictApply => traversable1NonEmptyArray.traverse1(dictApply)(Data$dSemigroup$dTraversable.identity),
  Foldable10: () => foldable1NonEmptyArray,
  Traversable1: () => Data$dTraversable.traversableArray
};
const eqNonEmptyArray = dictEq => ({eq: Data$dEq.eqArrayImpl(dictEq.eq)});
const eq1NonEmptyArray = Data$dEq.eq1Array;
const bindNonEmptyArray = Control$dBind.bindArray;
const applyNonEmptyArray = Control$dApply.applyArray;
const applicativeNonEmptyArray = Control$dApplicative.applicativeArray;
const altNonEmptyArray = Control$dAlt.altArray;
export {
  
  
  
  
  
  
  
  foldable1NonEmptyArray,
  
  
  
  
  
  
  
  
  
  
  
  
  
};
export * from "./foreign.js";
