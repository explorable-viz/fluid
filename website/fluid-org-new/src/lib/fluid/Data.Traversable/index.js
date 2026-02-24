import * as $runtime from "../runtime.js";
import * as Data$dConst from "../Data.Const/index.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dFunctor$dApp from "../Data.Functor.App/index.js";
import * as Data$dFunctor$dCompose from "../Data.Functor.Compose/index.js";
import * as Data$dFunctor$dProduct from "../Data.Functor.Product/index.js";
import * as Data$dIdentity from "../Data.Identity/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dMaybe$dFirst from "../Data.Maybe.First/index.js";
import * as Data$dMaybe$dLast from "../Data.Maybe.Last/index.js";
import * as Data$dMonoid$dAdditive from "../Data.Monoid.Additive/index.js";
import * as Data$dMonoid$dConj from "../Data.Monoid.Conj/index.js";
import * as Data$dMonoid$dDisj from "../Data.Monoid.Disj/index.js";
import * as Data$dMonoid$dDual from "../Data.Monoid.Dual/index.js";
import * as Data$dMonoid$dMultiplicative from "../Data.Monoid.Multiplicative/index.js";
import * as Data$dTraversable$dAccum$dInternal from "../Data.Traversable.Accum.Internal/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import {traverseArrayImpl} from "./foreign.js";
const identity = x => x;
const traverse = dict => dict.traverse;
const traversableTuple = {
  traverse: dictApplicative => f => v => dictApplicative.Apply0().Functor0().map(Data$dTuple.Tuple(v._1))(f(v._2)),
  sequence: dictApplicative => v => dictApplicative.Apply0().Functor0().map(Data$dTuple.Tuple(v._1))(v._2),
  Functor0: () => Data$dTuple.functorTuple,
  Foldable1: () => Data$dFoldable.foldableTuple
};
const traversableMultiplicative = {
  traverse: dictApplicative => f => v => dictApplicative.Apply0().Functor0().map(Data$dMonoid$dMultiplicative.Multiplicative)(f(v)),
  sequence: dictApplicative => v => dictApplicative.Apply0().Functor0().map(Data$dMonoid$dMultiplicative.Multiplicative)(v),
  Functor0: () => Data$dMonoid$dMultiplicative.functorMultiplicative,
  Foldable1: () => Data$dFoldable.foldableMultiplicative
};
const traversableMaybe = {
  traverse: dictApplicative => v => v1 => {
    if (v1.tag === "Nothing") { return dictApplicative.pure(Data$dMaybe.Nothing); }
    if (v1.tag === "Just") { return dictApplicative.Apply0().Functor0().map(Data$dMaybe.Just)(v(v1._1)); }
    $runtime.fail();
  },
  sequence: dictApplicative => v => {
    if (v.tag === "Nothing") { return dictApplicative.pure(Data$dMaybe.Nothing); }
    if (v.tag === "Just") { return dictApplicative.Apply0().Functor0().map(Data$dMaybe.Just)(v._1); }
    $runtime.fail();
  },
  Functor0: () => Data$dMaybe.functorMaybe,
  Foldable1: () => Data$dFoldable.foldableMaybe
};
const traversableIdentity = {
  traverse: dictApplicative => f => v => dictApplicative.Apply0().Functor0().map(Data$dIdentity.Identity)(f(v)),
  sequence: dictApplicative => v => dictApplicative.Apply0().Functor0().map(Data$dIdentity.Identity)(v),
  Functor0: () => Data$dIdentity.functorIdentity,
  Foldable1: () => Data$dFoldable.foldableIdentity
};
const traversableEither = {
  traverse: dictApplicative => v => v1 => {
    if (v1.tag === "Left") { return dictApplicative.pure(Data$dEither.$Either("Left", v1._1)); }
    if (v1.tag === "Right") { return dictApplicative.Apply0().Functor0().map(Data$dEither.Right)(v(v1._1)); }
    $runtime.fail();
  },
  sequence: dictApplicative => v => {
    if (v.tag === "Left") { return dictApplicative.pure(Data$dEither.$Either("Left", v._1)); }
    if (v.tag === "Right") { return dictApplicative.Apply0().Functor0().map(Data$dEither.Right)(v._1); }
    $runtime.fail();
  },
  Functor0: () => Data$dEither.functorEither,
  Foldable1: () => Data$dFoldable.foldableEither
};
const traversableDual = {
  traverse: dictApplicative => f => v => dictApplicative.Apply0().Functor0().map(Data$dMonoid$dDual.Dual)(f(v)),
  sequence: dictApplicative => v => dictApplicative.Apply0().Functor0().map(Data$dMonoid$dDual.Dual)(v),
  Functor0: () => Data$dMonoid$dDual.functorDual,
  Foldable1: () => Data$dFoldable.foldableDual
};
const traversableDisj = {
  traverse: dictApplicative => f => v => dictApplicative.Apply0().Functor0().map(Data$dMonoid$dDisj.Disj)(f(v)),
  sequence: dictApplicative => v => dictApplicative.Apply0().Functor0().map(Data$dMonoid$dDisj.Disj)(v),
  Functor0: () => Data$dMonoid$dDisj.functorDisj,
  Foldable1: () => Data$dFoldable.foldableDisj
};
const traversableConst = {
  traverse: dictApplicative => v => v1 => dictApplicative.pure(v1),
  sequence: dictApplicative => v => dictApplicative.pure(v),
  Functor0: () => Data$dConst.functorConst,
  Foldable1: () => Data$dFoldable.foldableConst
};
const traversableConj = {
  traverse: dictApplicative => f => v => dictApplicative.Apply0().Functor0().map(Data$dMonoid$dConj.Conj)(f(v)),
  sequence: dictApplicative => v => dictApplicative.Apply0().Functor0().map(Data$dMonoid$dConj.Conj)(v),
  Functor0: () => Data$dMonoid$dConj.functorConj,
  Foldable1: () => Data$dFoldable.foldableConj
};
const traversableCompose = dictTraversable => {
  const $0 = dictTraversable.Functor0();
  const $1 = dictTraversable.Foldable1();
  return dictTraversable1 => {
    const $2 = dictTraversable1.Functor0();
    const functorCompose1 = {map: f => v => $0.map($2.map(f))(v)};
    const $3 = dictTraversable1.Foldable1();
    const foldableCompose1 = {
      foldr: f => i => v => $1.foldr((() => {
        const $4 = $3.foldr(f);
        return b => a => $4(a)(b);
      })())(i)(v),
      foldl: f => i => v => $1.foldl($3.foldl(f))(i)(v),
      foldMap: dictMonoid => {
        const foldMap4 = $1.foldMap(dictMonoid);
        const foldMap5 = $3.foldMap(dictMonoid);
        return f => v => foldMap4(foldMap5(f))(v);
      }
    };
    return {
      traverse: dictApplicative => {
        const traverse4 = dictTraversable.traverse(dictApplicative);
        const traverse5 = dictTraversable1.traverse(dictApplicative);
        return f => v => dictApplicative.Apply0().Functor0().map(Data$dFunctor$dCompose.Compose)(traverse4(traverse5(f))(v));
      },
      sequence: dictApplicative => traversableCompose(dictTraversable)(dictTraversable1).traverse(dictApplicative)(identity),
      Functor0: () => functorCompose1,
      Foldable1: () => foldableCompose1
    };
  };
};
const traversableAdditive = {
  traverse: dictApplicative => f => v => dictApplicative.Apply0().Functor0().map(Data$dMonoid$dAdditive.Additive)(f(v)),
  sequence: dictApplicative => v => dictApplicative.Apply0().Functor0().map(Data$dMonoid$dAdditive.Additive)(v),
  Functor0: () => Data$dMonoid$dAdditive.functorAdditive,
  Foldable1: () => Data$dFoldable.foldableAdditive
};
const sequenceDefault = dictTraversable => dictApplicative => dictTraversable.traverse(dictApplicative)(identity);
const traversableArray = {
  traverse: dictApplicative => {
    const Apply0 = dictApplicative.Apply0();
    return traverseArrayImpl(Apply0.apply)(Apply0.Functor0().map)(dictApplicative.pure);
  },
  sequence: dictApplicative => traversableArray.traverse(dictApplicative)(identity),
  Functor0: () => Data$dFunctor.functorArray,
  Foldable1: () => Data$dFoldable.foldableArray
};
const sequence = dict => dict.sequence;
const traversableApp = dictTraversable => {
  const $0 = dictTraversable.Functor0();
  const $1 = dictTraversable.Foldable1();
  const foldableApp = {foldr: f => i => v => $1.foldr(f)(i)(v), foldl: f => i => v => $1.foldl(f)(i)(v), foldMap: dictMonoid => $1.foldMap(dictMonoid)};
  return {
    traverse: dictApplicative => {
      const traverse3 = dictTraversable.traverse(dictApplicative);
      return f => v => dictApplicative.Apply0().Functor0().map(Data$dFunctor$dApp.App)(traverse3(f)(v));
    },
    sequence: dictApplicative => {
      const sequence3 = dictTraversable.sequence(dictApplicative);
      return v => dictApplicative.Apply0().Functor0().map(Data$dFunctor$dApp.App)(sequence3(v));
    },
    Functor0: () => $0,
    Foldable1: () => foldableApp
  };
};
const traversableCoproduct = dictTraversable => {
  const $0 = dictTraversable.Functor0();
  const foldableCoproduct = Data$dFoldable.foldableCoproduct(dictTraversable.Foldable1());
  return dictTraversable1 => {
    const $1 = dictTraversable1.Functor0();
    const functorCoproduct1 = {
      map: f => v => {
        const $2 = $0.map(f);
        const $3 = $1.map(f);
        if (v.tag === "Left") { return Data$dEither.$Either("Left", $2(v._1)); }
        if (v.tag === "Right") { return Data$dEither.$Either("Right", $3(v._1)); }
        $runtime.fail();
      }
    };
    const foldableCoproduct1 = foldableCoproduct(dictTraversable1.Foldable1());
    return {
      traverse: dictApplicative => {
        const $2 = dictApplicative.Apply0().Functor0();
        const traverse4 = dictTraversable.traverse(dictApplicative);
        const traverse5 = dictTraversable1.traverse(dictApplicative);
        return f => {
          const $3 = $2.map(x => Data$dEither.$Either("Left", x));
          const $4 = traverse4(f);
          const $5 = $2.map(x => Data$dEither.$Either("Right", x));
          const $6 = traverse5(f);
          return v2 => {
            if (v2.tag === "Left") { return $3($4(v2._1)); }
            if (v2.tag === "Right") { return $5($6(v2._1)); }
            $runtime.fail();
          };
        };
      },
      sequence: dictApplicative => {
        const $2 = dictApplicative.Apply0().Functor0();
        const $3 = $2.map(x => Data$dEither.$Either("Left", x));
        const $4 = dictTraversable.sequence(dictApplicative);
        const $5 = $2.map(x => Data$dEither.$Either("Right", x));
        const $6 = dictTraversable1.sequence(dictApplicative);
        return v2 => {
          if (v2.tag === "Left") { return $3($4(v2._1)); }
          if (v2.tag === "Right") { return $5($6(v2._1)); }
          $runtime.fail();
        };
      },
      Functor0: () => functorCoproduct1,
      Foldable1: () => foldableCoproduct1
    };
  };
};
const traversableFirst = {
  traverse: dictApplicative => f => v => dictApplicative.Apply0().Functor0().map(Data$dMaybe$dFirst.First)(traversableMaybe.traverse(dictApplicative)(f)(v)),
  sequence: dictApplicative => v => dictApplicative.Apply0().Functor0().map(Data$dMaybe$dFirst.First)(traversableMaybe.sequence(dictApplicative)(v)),
  Functor0: () => Data$dMaybe.functorMaybe,
  Foldable1: () => Data$dFoldable.foldableFirst
};
const traversableLast = {
  traverse: dictApplicative => f => v => dictApplicative.Apply0().Functor0().map(Data$dMaybe$dLast.Last)(traversableMaybe.traverse(dictApplicative)(f)(v)),
  sequence: dictApplicative => v => dictApplicative.Apply0().Functor0().map(Data$dMaybe$dLast.Last)(traversableMaybe.sequence(dictApplicative)(v)),
  Functor0: () => Data$dMaybe.functorMaybe,
  Foldable1: () => Data$dFoldable.foldableLast
};
const traversableProduct = dictTraversable => {
  const $0 = dictTraversable.Functor0();
  const foldableProduct = Data$dFoldable.foldableProduct(dictTraversable.Foldable1());
  return dictTraversable1 => {
    const $1 = dictTraversable1.Functor0();
    const functorProduct1 = {map: f => v => Data$dTuple.$Tuple($0.map(f)(v._1), $1.map(f)(v._2))};
    const foldableProduct1 = foldableProduct(dictTraversable1.Foldable1());
    return {
      traverse: dictApplicative => {
        const $2 = dictApplicative.Apply0();
        const traverse4 = dictTraversable.traverse(dictApplicative);
        const traverse5 = dictTraversable1.traverse(dictApplicative);
        return f => v => $2.apply($2.Functor0().map(Data$dFunctor$dProduct.product)(traverse4(f)(v._1)))(traverse5(f)(v._2));
      },
      sequence: dictApplicative => {
        const $2 = dictApplicative.Apply0();
        const sequence4 = dictTraversable.sequence(dictApplicative);
        const sequence5 = dictTraversable1.sequence(dictApplicative);
        return v => $2.apply($2.Functor0().map(Data$dFunctor$dProduct.product)(sequence4(v._1)))(sequence5(v._2));
      },
      Functor0: () => functorProduct1,
      Foldable1: () => foldableProduct1
    };
  };
};
const traverseDefault = dictTraversable => dictApplicative => {
  const sequence3 = dictTraversable.sequence(dictApplicative);
  return f => ta => sequence3(dictTraversable.Functor0().map(f)(ta));
};
const mapAccumR = dictTraversable => {
  const traverse2 = dictTraversable.traverse(Data$dTraversable$dAccum$dInternal.applicativeStateR);
  return f => s0 => xs => traverse2(a => s => f(s)(a))(xs)(s0);
};
const scanr = dictTraversable => {
  const mapAccumR1 = mapAccumR(dictTraversable);
  return f => b0 => xs => mapAccumR1(b => a => {
    const b$p = f(a)(b);
    return {accum: b$p, value: b$p};
  })(b0)(xs).value;
};
const mapAccumL = dictTraversable => {
  const traverse2 = dictTraversable.traverse(Data$dTraversable$dAccum$dInternal.applicativeStateL);
  return f => s0 => xs => traverse2(a => s => f(s)(a))(xs)(s0);
};
const scanl = dictTraversable => {
  const mapAccumL1 = mapAccumL(dictTraversable);
  return f => b0 => xs => mapAccumL1(b => a => {
    const b$p = f(b)(a);
    return {accum: b$p, value: b$p};
  })(b0)(xs).value;
};
const $$for = dictApplicative => dictTraversable => {
  const traverse2 = dictTraversable.traverse(dictApplicative);
  return x => f => traverse2(f)(x);
};
export {
  
  identity,
  
  
  
  
  
  
  traversableAdditive,
  traversableApp,
  traversableArray,
  traversableCompose,
  traversableConj,
  traversableConst,
  traversableCoproduct,
  traversableDisj,
  traversableDual,
  traversableEither,
  traversableFirst,
  traversableIdentity,
  traversableLast,
  traversableMaybe,
  traversableMultiplicative,
  traversableProduct,
  traversableTuple,
  
  
};
export * from "./foreign.js";
