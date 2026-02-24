import * as Data$dOrdering from "../Data.Ordering/index.js";
const $Product2 = (_1, _2) => ({tag: "Product2", _1, _2});
const Product2 = value0 => value1 => $Product2(value0, value1);
const showProduct2 = dictShow => dictShow1 => ({show: v => "(Product2 " + dictShow.show(v._1) + " " + dictShow1.show(v._2) + ")"});
const profunctorProduct2 = dictProfunctor => dictProfunctor1 => ({dimap: f => g => v => $Product2(dictProfunctor.dimap(f)(g)(v._1), dictProfunctor1.dimap(f)(g)(v._2))});
const functorProduct2 = dictFunctor => dictFunctor1 => ({map: f => v => $Product2(dictFunctor.map(f)(v._1), dictFunctor1.map(f)(v._2))});
const eqProduct2 = dictEq => dictEq1 => ({eq: x => y => dictEq.eq(x._1)(y._1) && dictEq1.eq(x._2)(y._2)});
const ordProduct2 = dictOrd => {
  const $0 = dictOrd.Eq0();
  return dictOrd1 => {
    const $1 = dictOrd1.Eq0();
    const eqProduct22 = {eq: x => y => $0.eq(x._1)(y._1) && $1.eq(x._2)(y._2)};
    return {
      compare: x => y => {
        const v = dictOrd.compare(x._1)(y._1);
        if (v === "LT") { return Data$dOrdering.LT; }
        if (v === "GT") { return Data$dOrdering.GT; }
        return dictOrd1.compare(x._2)(y._2);
      },
      Eq0: () => eqProduct22
    };
  };
};
const bifunctorProduct2 = dictBifunctor => dictBifunctor1 => ({bimap: f => g => v => $Product2(dictBifunctor.bimap(f)(g)(v._1), dictBifunctor1.bimap(f)(g)(v._2))});
const biapplyProduct2 = dictBiapply => {
  const $0 = dictBiapply.Bifunctor0();
  return dictBiapply1 => {
    const $1 = dictBiapply1.Bifunctor0();
    const bifunctorProduct22 = {bimap: f => g => v => $Product2($0.bimap(f)(g)(v._1), $1.bimap(f)(g)(v._2))};
    return {biapply: v => v1 => $Product2(dictBiapply.biapply(v._1)(v1._1), dictBiapply1.biapply(v._2)(v1._2)), Bifunctor0: () => bifunctorProduct22};
  };
};
const biapplicativeProduct2 = dictBiapplicative => {
  const $0 = dictBiapplicative.Biapply0();
  const $1 = $0.Bifunctor0();
  return dictBiapplicative1 => {
    const $2 = dictBiapplicative1.Biapply0();
    const $3 = $2.Bifunctor0();
    const biapplyProduct22 = (() => {
      const bifunctorProduct22 = {bimap: f => g => v => $Product2($1.bimap(f)(g)(v._1), $3.bimap(f)(g)(v._2))};
      return {biapply: v => v1 => $Product2($0.biapply(v._1)(v1._1), $2.biapply(v._2)(v1._2)), Bifunctor0: () => bifunctorProduct22};
    })();
    return {bipure: a => b => $Product2(dictBiapplicative.bipure(a)(b), dictBiapplicative1.bipure(a)(b)), Biapply0: () => biapplyProduct22};
  };
};
export {$Product2, Product2,        };
