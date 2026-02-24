import * as $runtime from "../runtime.js";
import * as Data$dConst from "../Data.Const/index.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dIdentity from "../Data.Identity/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dMonoid$dAdditive from "../Data.Monoid.Additive/index.js";
import * as Data$dMonoid$dConj from "../Data.Monoid.Conj/index.js";
import * as Data$dMonoid$dDisj from "../Data.Monoid.Disj/index.js";
import * as Data$dMonoid$dDual from "../Data.Monoid.Dual/index.js";
import * as Data$dMonoid$dMultiplicative from "../Data.Monoid.Multiplicative/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import {mapWithIndexArray} from "./foreign.js";
const mapWithIndex = dict => dict.mapWithIndex;
const mapDefault = dictFunctorWithIndex => f => dictFunctorWithIndex.mapWithIndex(v => f);
const functorWithIndexTuple = {
  mapWithIndex: f => {
    const $0 = f();
    return m => Data$dTuple.$Tuple(m._1, $0(m._2));
  },
  Functor0: () => Data$dTuple.functorTuple
};
const functorWithIndexProduct = dictFunctorWithIndex => {
  const $0 = dictFunctorWithIndex.Functor0();
  return dictFunctorWithIndex1 => {
    const $1 = dictFunctorWithIndex1.Functor0();
    const functorProduct1 = {map: f => v => Data$dTuple.$Tuple($0.map(f)(v._1), $1.map(f)(v._2))};
    return {
      mapWithIndex: f => v => Data$dTuple.$Tuple(
        dictFunctorWithIndex.mapWithIndex(x => f(Data$dEither.$Either("Left", x)))(v._1),
        dictFunctorWithIndex1.mapWithIndex(x => f(Data$dEither.$Either("Right", x)))(v._2)
      ),
      Functor0: () => functorProduct1
    };
  };
};
const functorWithIndexMultiplicative = {mapWithIndex: f => f(), Functor0: () => Data$dMonoid$dMultiplicative.functorMultiplicative};
const functorWithIndexMaybe = {
  mapWithIndex: f => {
    const $0 = f();
    return v1 => {
      if (v1.tag === "Just") { return Data$dMaybe.$Maybe("Just", $0(v1._1)); }
      return Data$dMaybe.Nothing;
    };
  },
  Functor0: () => Data$dMaybe.functorMaybe
};
const functorWithIndexLast = {
  mapWithIndex: f => {
    const $0 = f();
    return v1 => {
      if (v1.tag === "Just") { return Data$dMaybe.$Maybe("Just", $0(v1._1)); }
      return Data$dMaybe.Nothing;
    };
  },
  Functor0: () => Data$dMaybe.functorMaybe
};
const functorWithIndexIdentity = {mapWithIndex: f => v => f()(v), Functor0: () => Data$dIdentity.functorIdentity};
const functorWithIndexFirst = {
  mapWithIndex: f => {
    const $0 = f();
    return v1 => {
      if (v1.tag === "Just") { return Data$dMaybe.$Maybe("Just", $0(v1._1)); }
      return Data$dMaybe.Nothing;
    };
  },
  Functor0: () => Data$dMaybe.functorMaybe
};
const functorWithIndexEither = {
  mapWithIndex: f => {
    const $0 = f();
    return m => {
      if (m.tag === "Left") { return Data$dEither.$Either("Left", m._1); }
      if (m.tag === "Right") { return Data$dEither.$Either("Right", $0(m._1)); }
      $runtime.fail();
    };
  },
  Functor0: () => Data$dEither.functorEither
};
const functorWithIndexDual = {mapWithIndex: f => f(), Functor0: () => Data$dMonoid$dDual.functorDual};
const functorWithIndexDisj = {mapWithIndex: f => f(), Functor0: () => Data$dMonoid$dDisj.functorDisj};
const functorWithIndexCoproduct = dictFunctorWithIndex => {
  const $0 = dictFunctorWithIndex.Functor0();
  return dictFunctorWithIndex1 => {
    const $1 = dictFunctorWithIndex1.Functor0();
    const functorCoproduct1 = {
      map: f => v => {
        const $2 = $0.map(f);
        const $3 = $1.map(f);
        if (v.tag === "Left") { return Data$dEither.$Either("Left", $2(v._1)); }
        if (v.tag === "Right") { return Data$dEither.$Either("Right", $3(v._1)); }
        $runtime.fail();
      }
    };
    return {
      mapWithIndex: f => v => {
        const $2 = dictFunctorWithIndex.mapWithIndex(x => f(Data$dEither.$Either("Left", x)));
        const $3 = dictFunctorWithIndex1.mapWithIndex(x => f(Data$dEither.$Either("Right", x)));
        if (v.tag === "Left") { return Data$dEither.$Either("Left", $2(v._1)); }
        if (v.tag === "Right") { return Data$dEither.$Either("Right", $3(v._1)); }
        $runtime.fail();
      },
      Functor0: () => functorCoproduct1
    };
  };
};
const functorWithIndexConst = {mapWithIndex: v => v1 => v1, Functor0: () => Data$dConst.functorConst};
const functorWithIndexConj = {mapWithIndex: f => f(), Functor0: () => Data$dMonoid$dConj.functorConj};
const functorWithIndexCompose = dictFunctorWithIndex => {
  const $0 = dictFunctorWithIndex.Functor0();
  return dictFunctorWithIndex1 => {
    const $1 = dictFunctorWithIndex1.Functor0();
    const functorCompose1 = {map: f => v => $0.map($1.map(f))(v)};
    return {
      mapWithIndex: f => v => dictFunctorWithIndex.mapWithIndex(x => dictFunctorWithIndex1.mapWithIndex(b => f(Data$dTuple.$Tuple(x, b))))(v),
      Functor0: () => functorCompose1
    };
  };
};
const functorWithIndexArray = {mapWithIndex: mapWithIndexArray, Functor0: () => Data$dFunctor.functorArray};
const functorWithIndexApp = dictFunctorWithIndex => {
  const $0 = dictFunctorWithIndex.Functor0();
  return {mapWithIndex: f => v => dictFunctorWithIndex.mapWithIndex(f)(v), Functor0: () => $0};
};
const functorWithIndexAdditive = {mapWithIndex: f => f(), Functor0: () => Data$dMonoid$dAdditive.functorAdditive};
export {
  functorWithIndexAdditive,
  
  functorWithIndexArray,
  
  functorWithIndexConj,
  functorWithIndexConst,
  functorWithIndexCoproduct,
  functorWithIndexDisj,
  functorWithIndexDual,
  functorWithIndexEither,
  functorWithIndexFirst,
  functorWithIndexIdentity,
  functorWithIndexLast,
  functorWithIndexMaybe,
  functorWithIndexMultiplicative,
  
  functorWithIndexTuple,
  
  
};
export * from "./foreign.js";
