import * as $runtime from "../runtime.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
const Coproduct = x => x;
const showCoproduct = dictShow => dictShow1 => (
  {
    show: v => {
      if (v.tag === "Left") { return "(left " + dictShow.show(v._1) + ")"; }
      if (v.tag === "Right") { return "(right " + dictShow1.show(v._1) + ")"; }
      $runtime.fail();
    }
  }
);
const right = ga => Data$dEither.$Either("Right", ga);
const newtypeCoproduct = {Coercible0: () => {}};
const left = fa => Data$dEither.$Either("Left", fa);
const functorCoproduct = dictFunctor => dictFunctor1 => (
  {
    map: f => v => {
      const $0 = dictFunctor.map(f);
      const $1 = dictFunctor1.map(f);
      if (v.tag === "Left") { return Data$dEither.$Either("Left", $0(v._1)); }
      if (v.tag === "Right") { return Data$dEither.$Either("Right", $1(v._1)); }
      $runtime.fail();
    }
  }
);
const eq1Coproduct = dictEq1 => dictEq11 => (
  {
    eq1: dictEq => {
      const eq12 = dictEq1.eq1(dictEq);
      const eq13 = dictEq11.eq1(dictEq);
      return v => v1 => {
        if (v.tag === "Left") { return v1.tag === "Left" && eq12(v._1)(v1._1); }
        return v.tag === "Right" && v1.tag === "Right" && eq13(v._1)(v1._1);
      };
    }
  }
);
const eqCoproduct = dictEq1 => dictEq11 => dictEq => (
  {
    eq: (() => {
      const eq12 = dictEq1.eq1(dictEq);
      const eq13 = dictEq11.eq1(dictEq);
      return v => v1 => {
        if (v.tag === "Left") { return v1.tag === "Left" && eq12(v._1)(v1._1); }
        return v.tag === "Right" && v1.tag === "Right" && eq13(v._1)(v1._1);
      };
    })()
  }
);
const ord1Coproduct = dictOrd1 => {
  const $0 = dictOrd1.Eq10();
  return dictOrd11 => {
    const $1 = dictOrd11.Eq10();
    const eq1Coproduct2 = {
      eq1: dictEq => {
        const eq12 = $0.eq1(dictEq);
        const eq13 = $1.eq1(dictEq);
        return v => v1 => {
          if (v.tag === "Left") { return v1.tag === "Left" && eq12(v._1)(v1._1); }
          return v.tag === "Right" && v1.tag === "Right" && eq13(v._1)(v1._1);
        };
      }
    };
    return {
      compare1: dictOrd => {
        const compare12 = dictOrd1.compare1(dictOrd);
        const compare13 = dictOrd11.compare1(dictOrd);
        return v => v1 => {
          if (v.tag === "Left") {
            if (v1.tag === "Left") { return compare12(v._1)(v1._1); }
            return Data$dOrdering.LT;
          }
          if (v1.tag === "Left") { return Data$dOrdering.GT; }
          if (v.tag === "Right" && v1.tag === "Right") { return compare13(v._1)(v1._1); }
          $runtime.fail();
        };
      },
      Eq10: () => eq1Coproduct2
    };
  };
};
const ordCoproduct = dictOrd1 => {
  const ord1Coproduct1 = ord1Coproduct(dictOrd1);
  const $0 = dictOrd1.Eq10();
  return dictOrd11 => {
    const $1 = dictOrd11.Eq10();
    return dictOrd => {
      const $2 = dictOrd.Eq0();
      const eqCoproduct3 = {
        eq: (() => {
          const eq12 = $0.eq1($2);
          const eq13 = $1.eq1($2);
          return v => v1 => {
            if (v.tag === "Left") { return v1.tag === "Left" && eq12(v._1)(v1._1); }
            return v.tag === "Right" && v1.tag === "Right" && eq13(v._1)(v1._1);
          };
        })()
      };
      return {compare: ord1Coproduct1(dictOrd11).compare1(dictOrd), Eq0: () => eqCoproduct3};
    };
  };
};
const coproduct = v => v1 => v2 => {
  if (v2.tag === "Left") { return v(v2._1); }
  if (v2.tag === "Right") { return v1(v2._1); }
  $runtime.fail();
};
const extendCoproduct = dictExtend => {
  const $0 = dictExtend.Functor0();
  return dictExtend1 => {
    const $1 = dictExtend1.Functor0();
    const functorCoproduct2 = {
      map: f => v => {
        const $2 = $0.map(f);
        const $3 = $1.map(f);
        if (v.tag === "Left") { return Data$dEither.$Either("Left", $2(v._1)); }
        if (v.tag === "Right") { return Data$dEither.$Either("Right", $3(v._1)); }
        $runtime.fail();
      }
    };
    return {
      extend: f => {
        const $2 = dictExtend.extend(x => f(Data$dEither.$Either("Left", x)));
        const $3 = dictExtend1.extend(x => f(Data$dEither.$Either("Right", x)));
        return v2 => {
          if (v2.tag === "Left") { return Data$dEither.$Either("Left", $2(v2._1)); }
          if (v2.tag === "Right") { return Data$dEither.$Either("Right", $3(v2._1)); }
          $runtime.fail();
        };
      },
      Functor0: () => functorCoproduct2
    };
  };
};
const comonadCoproduct = dictComonad => {
  const extendCoproduct1 = extendCoproduct(dictComonad.Extend0());
  return dictComonad1 => {
    const extendCoproduct2 = extendCoproduct1(dictComonad1.Extend0());
    return {
      extract: v2 => {
        if (v2.tag === "Left") { return dictComonad.extract(v2._1); }
        if (v2.tag === "Right") { return dictComonad1.extract(v2._1); }
        $runtime.fail();
      },
      Extend0: () => extendCoproduct2
    };
  };
};
const bihoistCoproduct = natF => natG => v => {
  if (v.tag === "Left") { return Data$dEither.$Either("Left", natF(v._1)); }
  if (v.tag === "Right") { return Data$dEither.$Either("Right", natG(v._1)); }
  $runtime.fail();
};
export {
  Coproduct,
  bihoistCoproduct,
  comonadCoproduct,
  coproduct,
  eq1Coproduct,
  eqCoproduct,
  extendCoproduct,
  functorCoproduct,
  left,
  newtypeCoproduct,
  ord1Coproduct,
  ordCoproduct,
  right,
  showCoproduct
};
