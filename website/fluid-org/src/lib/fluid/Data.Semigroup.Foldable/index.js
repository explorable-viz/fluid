import * as $runtime from "../runtime.js";
import * as Control$dApply from "../Control.Apply/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dFunction from "../Data.Function/index.js";
import * as Unsafe$dCoerce from "../Unsafe.Coerce/index.js";
const $FoldRight1 = (_1, _2) => ({tag: "FoldRight1", _1, _2});
const identity = x => x;
const FoldRight1 = value0 => value1 => $FoldRight1(value0, value1);
const semigroupAct = dictApply => ({append: v => v1 => dictApply.apply(dictApply.Functor0().map(v$1 => Control$dApply.identity)(v))(v1)});
const mkFoldRight1 = /* #__PURE__ */ FoldRight1(Data$dFunction.const);
const foldr1 = dict => dict.foldr1;
const foldl1 = dict => dict.foldl1;
const maximumBy = dictFoldable1 => cmp => dictFoldable1.foldl1(x => y => {
  if (cmp(x)(y) === "GT") { return x; }
  return y;
});
const minimumBy = dictFoldable1 => cmp => dictFoldable1.foldl1(x => y => {
  if (cmp(x)(y) === "LT") { return x; }
  return y;
});
const foldableTuple = {foldMap1: dictSemigroup => f => v => f(v._2), foldr1: v => v1 => v1._2, foldl1: v => v1 => v1._2, Foldable0: () => Data$dFoldable.foldableTuple};
const foldableMultiplicative = {foldr1: v => v1 => v1, foldl1: v => v1 => v1, foldMap1: dictSemigroup => f => v => f(v), Foldable0: () => Data$dFoldable.foldableMultiplicative};
const foldableIdentity = {foldMap1: dictSemigroup => f => v => f(v), foldl1: v => v1 => v1, foldr1: v => v1 => v1, Foldable0: () => Data$dFoldable.foldableIdentity};
const foldableDual = {foldr1: v => v1 => v1, foldl1: v => v1 => v1, foldMap1: dictSemigroup => f => v => f(v), Foldable0: () => Data$dFoldable.foldableDual};
const foldRight1Semigroup = {
  append: v => v1 => {
    const $0 = v._2;
    return $FoldRight1(a => f => v._1(f($0)(v1._1(a)(f)))(f), v1._2);
  }
};
const semigroupDual = {
  append: v => v1 => {
    const $0 = v1._2;
    return $FoldRight1(a => f => v1._1(f($0)(v._1(a)(f)))(f), v._2);
  }
};
const foldMap1DefaultR = dictFoldable1 => dictFunctor => dictSemigroup => {
  const append = dictSemigroup.append;
  return f => {
    const $0 = dictFunctor.map(f);
    const $1 = dictFoldable1.foldr1(append);
    return x => $1($0(x));
  };
};
const foldMap1DefaultL = dictFoldable1 => dictFunctor => dictSemigroup => {
  const append = dictSemigroup.append;
  return f => {
    const $0 = dictFunctor.map(f);
    const $1 = dictFoldable1.foldl1(append);
    return x => $1($0(x));
  };
};
const foldMap1 = dict => dict.foldMap1;
const foldl1Default = dictFoldable1 => {
  const $0 = dictFoldable1.foldMap1(semigroupDual)(mkFoldRight1);
  return x => a => {
    const $1 = $0(a);
    return $1._1($1._2)(b => a$1 => x(a$1)(b));
  };
};
const foldr1Default = dictFoldable1 => {
  const $0 = dictFoldable1.foldMap1(foldRight1Semigroup)(mkFoldRight1);
  return b => a => {
    const $1 = $0(a);
    return $1._1($1._2)(b);
  };
};
const intercalateMap = dictFoldable1 => dictSemigroup => {
  const foldMap12 = dictFoldable1.foldMap1({append: v => v1 => j => dictSemigroup.append(v(j))(dictSemigroup.append(j)(v1(j)))});
  return j => f => foldable => foldMap12(x => {
    const $0 = f(x);
    return v => $0;
  })(foldable)(j);
};
const intercalate = dictFoldable1 => dictSemigroup => {
  const foldMap12 = dictFoldable1.foldMap1({append: v => v1 => j => dictSemigroup.append(v(j))(dictSemigroup.append(j)(v1(j)))});
  return a => foldable => foldMap12(x => v => x)(foldable)(a);
};
const maximum = dictOrd => {
  const semigroupMax = {
    append: v => v1 => {
      const v$1 = dictOrd.compare(v)(v1);
      if (v$1 === "LT") { return v1; }
      if (v$1 === "EQ") { return v; }
      if (v$1 === "GT") { return v; }
      $runtime.fail();
    }
  };
  return dictFoldable1 => dictFoldable1.foldMap1(semigroupMax)(Unsafe$dCoerce.unsafeCoerce);
};
const minimum = dictOrd => {
  const semigroupMin = {
    append: v => v1 => {
      const v$1 = dictOrd.compare(v)(v1);
      if (v$1 === "LT") { return v; }
      if (v$1 === "EQ") { return v; }
      if (v$1 === "GT") { return v1; }
      $runtime.fail();
    }
  };
  return dictFoldable1 => dictFoldable1.foldMap1(semigroupMin)(Unsafe$dCoerce.unsafeCoerce);
};
const traverse1_ = dictFoldable1 => dictApply => {
  const $0 = dictApply.Functor0();
  const foldMap12 = dictFoldable1.foldMap1(semigroupAct(dictApply));
  return f => t => $0.map(v => {})(foldMap12(x => f(x))(t));
};
const for1_ = dictFoldable1 => dictApply => {
  const $0 = traverse1_(dictFoldable1)(dictApply);
  return b => a => $0(a)(b);
};
const sequence1_ = dictFoldable1 => dictApply => traverse1_(dictFoldable1)(dictApply)(identity);
const fold1 = dictFoldable1 => dictSemigroup => dictFoldable1.foldMap1(dictSemigroup)(identity);
export {
  
  
  
  
  
  
  
  foldableDual,
  foldableIdentity,
  foldableMultiplicative,
  foldableTuple,
  
  
  
  
  
  identity,
  
  
  maximum,
  
  minimum,
  
  
  
  
  
  
};
