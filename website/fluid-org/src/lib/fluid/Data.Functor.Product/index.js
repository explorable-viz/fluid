import * as Data$dTuple from "../Data.Tuple/index.js";
const Product = x => x;
const showProduct = dictShow => dictShow1 => ({show: v => "(product " + dictShow.show(v._1) + " " + dictShow1.show(v._2) + ")"});
const product = fa => ga => Data$dTuple.$Tuple(fa, ga);
const newtypeProduct = {Coercible0: () => {}};
const functorProduct = dictFunctor => dictFunctor1 => ({map: f => v => Data$dTuple.$Tuple(dictFunctor.map(f)(v._1), dictFunctor1.map(f)(v._2))});
const eq1Product = dictEq1 => dictEq11 => (
  {
    eq1: dictEq => {
      const eq12 = dictEq1.eq1(dictEq);
      const eq13 = dictEq11.eq1(dictEq);
      return v => v1 => eq12(v._1)(v1._1) && eq13(v._2)(v1._2);
    }
  }
);
const eqProduct = dictEq1 => dictEq11 => dictEq => (
  {
    eq: (() => {
      const eq12 = dictEq1.eq1(dictEq);
      const eq13 = dictEq11.eq1(dictEq);
      return v => v1 => eq12(v._1)(v1._1) && eq13(v._2)(v1._2);
    })()
  }
);
const ord1Product = dictOrd1 => {
  const $0 = dictOrd1.Eq10();
  return dictOrd11 => {
    const $1 = dictOrd11.Eq10();
    const eq1Product2 = {
      eq1: dictEq => {
        const eq12 = $0.eq1(dictEq);
        const eq13 = $1.eq1(dictEq);
        return v => v1 => eq12(v._1)(v1._1) && eq13(v._2)(v1._2);
      }
    };
    return {
      compare1: dictOrd => {
        const compare12 = dictOrd1.compare1(dictOrd);
        const compare13 = dictOrd11.compare1(dictOrd);
        return v => v1 => {
          const v2 = compare12(v._1)(v1._1);
          if (v2 === "EQ") { return compare13(v._2)(v1._2); }
          return v2;
        };
      },
      Eq10: () => eq1Product2
    };
  };
};
const ordProduct = dictOrd1 => {
  const ord1Product1 = ord1Product(dictOrd1);
  const $0 = dictOrd1.Eq10();
  return dictOrd11 => {
    const $1 = dictOrd11.Eq10();
    return dictOrd => {
      const $2 = dictOrd.Eq0();
      const eqProduct3 = {
        eq: (() => {
          const eq12 = $0.eq1($2);
          const eq13 = $1.eq1($2);
          return v => v1 => eq12(v._1)(v1._1) && eq13(v._2)(v1._2);
        })()
      };
      return {compare: ord1Product1(dictOrd11).compare1(dictOrd), Eq0: () => eqProduct3};
    };
  };
};
const bihoistProduct = natF => natG => v => Data$dTuple.$Tuple(natF(v._1), natG(v._2));
const applyProduct = dictApply => {
  const $0 = dictApply.Functor0();
  return dictApply1 => {
    const $1 = dictApply1.Functor0();
    const functorProduct2 = {map: f => v => Data$dTuple.$Tuple($0.map(f)(v._1), $1.map(f)(v._2))};
    return {apply: v => v1 => Data$dTuple.$Tuple(dictApply.apply(v._1)(v1._1), dictApply1.apply(v._2)(v1._2)), Functor0: () => functorProduct2};
  };
};
const bindProduct = dictBind => {
  const $0 = dictBind.Apply0();
  const $1 = $0.Functor0();
  return dictBind1 => {
    const $2 = dictBind1.Apply0();
    const $3 = $2.Functor0();
    const applyProduct2 = (() => {
      const functorProduct2 = {map: f => v => Data$dTuple.$Tuple($1.map(f)(v._1), $3.map(f)(v._2))};
      return {apply: v => v1 => Data$dTuple.$Tuple($0.apply(v._1)(v1._1), $2.apply(v._2)(v1._2)), Functor0: () => functorProduct2};
    })();
    return {bind: v => f => Data$dTuple.$Tuple(dictBind.bind(v._1)(x => f(x)._1), dictBind1.bind(v._2)(x => f(x)._2)), Apply0: () => applyProduct2};
  };
};
const applicativeProduct = dictApplicative => {
  const $0 = dictApplicative.Apply0();
  const $1 = $0.Functor0();
  return dictApplicative1 => {
    const $2 = dictApplicative1.Apply0();
    const $3 = $2.Functor0();
    const applyProduct2 = (() => {
      const functorProduct2 = {map: f => v => Data$dTuple.$Tuple($1.map(f)(v._1), $3.map(f)(v._2))};
      return {apply: v => v1 => Data$dTuple.$Tuple($0.apply(v._1)(v1._1), $2.apply(v._2)(v1._2)), Functor0: () => functorProduct2};
    })();
    return {pure: a => Data$dTuple.$Tuple(dictApplicative.pure(a), dictApplicative1.pure(a)), Apply0: () => applyProduct2};
  };
};
const monadProduct = dictMonad => {
  const applicativeProduct1 = applicativeProduct(dictMonad.Applicative0());
  const bindProduct1 = bindProduct(dictMonad.Bind1());
  return dictMonad1 => {
    const applicativeProduct2 = applicativeProduct1(dictMonad1.Applicative0());
    const bindProduct2 = bindProduct1(dictMonad1.Bind1());
    return {Applicative0: () => applicativeProduct2, Bind1: () => bindProduct2};
  };
};
export {
  
  
  
  
  
  
  
  
  
  
  
  
  product,
  
};
