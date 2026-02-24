import * as $runtime from "../runtime.js";
import * as Data$dBounded from "../Data.Bounded/index.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dEq from "../Data.Eq/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import {fromCharCode, toCharCode} from "./foreign.js";
const Cardinality = x => x;
const toEnum = dict => dict.toEnum;
const succ = dict => dict.succ;
const upFromIncluding = dictEnum => dictUnfoldable1 => dictUnfoldable1.unfoldr1(x => Data$dTuple.$Tuple(x, dictEnum.succ(x)));
const showCardinality = {show: v => "(Cardinality " + Data$dShow.showIntImpl(v) + ")"};
const pred = dict => dict.pred;
const ordCardinality = Data$dOrd.ordInt;
const newtypeCardinality = {Coercible0: () => {}};
const fromEnum = dict => dict.fromEnum;
const toEnumWithDefaults = dictBoundedEnum => {
  const bottom2 = dictBoundedEnum.Bounded0().bottom;
  return low => high => x => {
    const v = dictBoundedEnum.toEnum(x);
    if (v.tag === "Just") { return v._1; }
    if (v.tag === "Nothing") {
      if (x < dictBoundedEnum.fromEnum(bottom2)) { return low; }
      return high;
    }
    $runtime.fail();
  };
};
const eqCardinality = Data$dEq.eqInt;
const enumUnit = {succ: v => Data$dMaybe.Nothing, pred: v => Data$dMaybe.Nothing, Ord0: () => Data$dOrd.ordUnit};
const enumTuple = dictEnum => {
  const $0 = dictEnum.Ord0();
  const $1 = $0.Eq0();
  return dictBoundedEnum => {
    const Bounded0 = dictBoundedEnum.Bounded0();
    const bottom2 = Bounded0.bottom;
    const Enum1 = dictBoundedEnum.Enum1();
    const top2 = Bounded0.top;
    const $2 = Enum1.Ord0();
    const $3 = $2.Eq0();
    const ordTuple1 = (() => {
      const eqTuple2 = {eq: x => y => $1.eq(x._1)(y._1) && $3.eq(x._2)(y._2)};
      return {
        compare: x => y => {
          const v = $0.compare(x._1)(y._1);
          if (v === "LT") { return Data$dOrdering.LT; }
          if (v === "GT") { return Data$dOrdering.GT; }
          return $2.compare(x._2)(y._2);
        },
        Eq0: () => eqTuple2
      };
    })();
    return {
      succ: v => {
        const $4 = dictEnum.succ(v._1);
        const $5 = $4.tag === "Just" ? Data$dMaybe.$Maybe("Just", Data$dTuple.$Tuple($4._1, bottom2)) : Data$dMaybe.Nothing;
        const $6 = Data$dTuple.Tuple(v._1);
        const $7 = Enum1.succ(v._2);
        if ($7.tag === "Nothing") { return $5; }
        if ($7.tag === "Just") { return Data$dMaybe.$Maybe("Just", $6($7._1)); }
        $runtime.fail();
      },
      pred: v => {
        const $4 = dictEnum.pred(v._1);
        const $5 = $4.tag === "Just" ? Data$dMaybe.$Maybe("Just", Data$dTuple.$Tuple($4._1, top2)) : Data$dMaybe.Nothing;
        const $6 = Data$dTuple.Tuple(v._1);
        const $7 = Enum1.pred(v._2);
        if ($7.tag === "Nothing") { return $5; }
        if ($7.tag === "Just") { return Data$dMaybe.$Maybe("Just", $6($7._1)); }
        $runtime.fail();
      },
      Ord0: () => ordTuple1
    };
  };
};
const enumOrdering = {
  succ: v => {
    if (v === "LT") { return Data$dMaybe.$Maybe("Just", Data$dOrdering.EQ); }
    if (v === "EQ") { return Data$dMaybe.$Maybe("Just", Data$dOrdering.GT); }
    if (v === "GT") { return Data$dMaybe.Nothing; }
    $runtime.fail();
  },
  pred: v => {
    if (v === "LT") { return Data$dMaybe.Nothing; }
    if (v === "EQ") { return Data$dMaybe.$Maybe("Just", Data$dOrdering.LT); }
    if (v === "GT") { return Data$dMaybe.$Maybe("Just", Data$dOrdering.EQ); }
    $runtime.fail();
  },
  Ord0: () => Data$dOrd.ordOrdering
};
const enumMaybe = dictBoundedEnum => {
  const bottom2 = dictBoundedEnum.Bounded0().bottom;
  const Enum1 = dictBoundedEnum.Enum1();
  const $0 = Enum1.Ord0();
  const $1 = $0.Eq0();
  const ordMaybe = (() => {
    const eqMaybe1 = {
      eq: x => y => {
        if (x.tag === "Nothing") { return y.tag === "Nothing"; }
        return x.tag === "Just" && y.tag === "Just" && $1.eq(x._1)(y._1);
      }
    };
    return {
      compare: x => y => {
        if (x.tag === "Nothing") {
          if (y.tag === "Nothing") { return Data$dOrdering.EQ; }
          return Data$dOrdering.LT;
        }
        if (y.tag === "Nothing") { return Data$dOrdering.GT; }
        if (x.tag === "Just" && y.tag === "Just") { return $0.compare(x._1)(y._1); }
        $runtime.fail();
      },
      Eq0: () => eqMaybe1
    };
  })();
  return {
    succ: v => {
      if (v.tag === "Nothing") { return Data$dMaybe.$Maybe("Just", Data$dMaybe.$Maybe("Just", bottom2)); }
      if (v.tag === "Just") {
        const $2 = Enum1.succ(v._1);
        if ($2.tag === "Just") { return Data$dMaybe.$Maybe("Just", Data$dMaybe.$Maybe("Just", $2._1)); }
        return Data$dMaybe.Nothing;
      }
      $runtime.fail();
    },
    pred: v => {
      if (v.tag === "Nothing") { return Data$dMaybe.Nothing; }
      if (v.tag === "Just") { return Data$dMaybe.$Maybe("Just", Enum1.pred(v._1)); }
      $runtime.fail();
    },
    Ord0: () => ordMaybe
  };
};
const enumInt = {
  succ: n => {
    if (n < 2147483647) { return Data$dMaybe.$Maybe("Just", n + 1 | 0); }
    return Data$dMaybe.Nothing;
  },
  pred: n => {
    if (n > -2147483648) { return Data$dMaybe.$Maybe("Just", n - 1 | 0); }
    return Data$dMaybe.Nothing;
  },
  Ord0: () => Data$dOrd.ordInt
};
const enumFromTo = dictEnum => {
  const Ord0 = dictEnum.Ord0();
  return dictUnfoldable1 => v => v1 => {
    if (Ord0.Eq0().eq(v)(v1)) {
      return dictUnfoldable1.unfoldr1(i => {
        if (i <= 0) { return Data$dTuple.$Tuple(v, Data$dMaybe.Nothing); }
        return Data$dTuple.$Tuple(v, Data$dMaybe.$Maybe("Just", i - 1 | 0));
      })(0);
    }
    if (Ord0.compare(v)(v1) === "LT") {
      return dictUnfoldable1.unfoldr1(a => Data$dTuple.$Tuple(
        a,
        (() => {
          const $0 = dictEnum.succ(a);
          if ($0.tag === "Just") {
            if (Ord0.compare($0._1)(v1) !== "GT") { return Data$dMaybe.$Maybe("Just", $0._1); }
            return Data$dMaybe.Nothing;
          }
          if ($0.tag === "Nothing") { return Data$dMaybe.Nothing; }
          $runtime.fail();
        })()
      ))(v);
    }
    return dictUnfoldable1.unfoldr1(a => Data$dTuple.$Tuple(
      a,
      (() => {
        const $0 = dictEnum.pred(a);
        if ($0.tag === "Just") {
          if (Ord0.compare($0._1)(v1) !== "LT") { return Data$dMaybe.$Maybe("Just", $0._1); }
          return Data$dMaybe.Nothing;
        }
        if ($0.tag === "Nothing") { return Data$dMaybe.Nothing; }
        $runtime.fail();
      })()
    ))(v);
  };
};
const enumFromThenTo = dictUnfoldable => dictFunctor => dictBoundedEnum => a => b => c => {
  const a$p = dictBoundedEnum.fromEnum(a);
  return dictFunctor.map(x => {
    const $0 = dictBoundedEnum.toEnum(x);
    if ($0.tag === "Just") { return $0._1; }
    $runtime.fail();
  })(dictUnfoldable.unfoldr((() => {
    const $0 = dictBoundedEnum.fromEnum(b) - a$p | 0;
    const $1 = dictBoundedEnum.fromEnum(c);
    return e => {
      if (e <= $1) { return Data$dMaybe.$Maybe("Just", Data$dTuple.$Tuple(e, e + $0 | 0)); }
      return Data$dMaybe.Nothing;
    };
  })())(a$p));
};
const enumEither = dictBoundedEnum => {
  const Enum1 = dictBoundedEnum.Enum1();
  const top2 = dictBoundedEnum.Bounded0().top;
  const ordEither = Data$dEither.ordEither(Enum1.Ord0());
  return dictBoundedEnum1 => {
    const bottom2 = dictBoundedEnum1.Bounded0().bottom;
    const Enum11 = dictBoundedEnum1.Enum1();
    const ordEither1 = ordEither(Enum11.Ord0());
    return {
      succ: v => {
        if (v.tag === "Left") {
          const $0 = Enum1.succ(v._1);
          if ($0.tag === "Nothing") { return Data$dMaybe.$Maybe("Just", Data$dEither.$Either("Right", bottom2)); }
          if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", Data$dEither.$Either("Left", $0._1)); }
          $runtime.fail();
        }
        if (v.tag === "Right") {
          const $0 = Enum11.succ(v._1);
          if ($0.tag === "Nothing") { return Data$dMaybe.Nothing; }
          if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", Data$dEither.$Either("Right", $0._1)); }
        }
        $runtime.fail();
      },
      pred: v => {
        if (v.tag === "Left") {
          const $0 = Enum1.pred(v._1);
          if ($0.tag === "Nothing") { return Data$dMaybe.Nothing; }
          if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", Data$dEither.$Either("Left", $0._1)); }
          $runtime.fail();
        }
        if (v.tag === "Right") {
          const $0 = Enum11.pred(v._1);
          if ($0.tag === "Nothing") { return Data$dMaybe.$Maybe("Just", Data$dEither.$Either("Left", top2)); }
          if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", Data$dEither.$Either("Right", $0._1)); }
        }
        $runtime.fail();
      },
      Ord0: () => ordEither1
    };
  };
};
const enumBoolean = {
  succ: v => {
    if (!v) { return Data$dMaybe.$Maybe("Just", true); }
    return Data$dMaybe.Nothing;
  },
  pred: v => {
    if (v) { return Data$dMaybe.$Maybe("Just", false); }
    return Data$dMaybe.Nothing;
  },
  Ord0: () => Data$dOrd.ordBoolean
};
const downFromIncluding = dictEnum => dictUnfoldable1 => dictUnfoldable1.unfoldr1(x => Data$dTuple.$Tuple(x, dictEnum.pred(x)));
const downFrom = dictEnum => dictUnfoldable => dictUnfoldable.unfoldr(x => {
  const $0 = dictEnum.pred(x);
  if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", Data$dTuple.$Tuple($0._1, $0._1)); }
  return Data$dMaybe.Nothing;
});
const upFrom = dictEnum => dictUnfoldable => dictUnfoldable.unfoldr(x => {
  const $0 = dictEnum.succ(x);
  if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", Data$dTuple.$Tuple($0._1, $0._1)); }
  return Data$dMaybe.Nothing;
});
const defaultToEnum = dictBounded => {
  const bottom2 = dictBounded.bottom;
  return dictEnum => i$p => {
    const go = go$a0$copy => go$a1$copy => {
      let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
      while (go$c) {
        const i = go$a0, x = go$a1;
        if (i === 0) {
          go$c = false;
          go$r = Data$dMaybe.$Maybe("Just", x);
          continue;
        }
        const v = dictEnum.succ(x);
        if (v.tag === "Just") {
          go$a0 = i - 1 | 0;
          go$a1 = v._1;
          continue;
        }
        if (v.tag === "Nothing") {
          go$c = false;
          go$r = Data$dMaybe.Nothing;
          continue;
        }
        $runtime.fail();
      }
      return go$r;
    };
    if (i$p < 0) { return Data$dMaybe.Nothing; }
    return go(i$p)(bottom2);
  };
};
const defaultSucc = toEnum$p => fromEnum$p => a => toEnum$p(fromEnum$p(a) + 1 | 0);
const defaultPred = toEnum$p => fromEnum$p => a => toEnum$p(fromEnum$p(a) - 1 | 0);
const defaultFromEnum = dictEnum => {
  const go = go$a0$copy => go$a1$copy => {
    let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
    while (go$c) {
      const i = go$a0, x = go$a1;
      const v = dictEnum.pred(x);
      if (v.tag === "Just") {
        go$a0 = i + 1 | 0;
        go$a1 = v._1;
        continue;
      }
      if (v.tag === "Nothing") {
        go$c = false;
        go$r = i;
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  return go(0);
};
const defaultCardinality = dictBounded => {
  const bottom2 = dictBounded.bottom;
  return dictEnum => {
    const go = go$a0$copy => go$a1$copy => {
      let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
      while (go$c) {
        const i = go$a0, x = go$a1;
        const v = dictEnum.succ(x);
        if (v.tag === "Just") {
          go$a0 = i + 1 | 0;
          go$a1 = v._1;
          continue;
        }
        if (v.tag === "Nothing") {
          go$c = false;
          go$r = i;
          continue;
        }
        $runtime.fail();
      }
      return go$r;
    };
    return go(1)(bottom2);
  };
};
const charToEnum = v => {
  if (v >= 0 && v <= 65535) { return Data$dMaybe.$Maybe("Just", fromCharCode(v)); }
  return Data$dMaybe.Nothing;
};
const enumChar = {
  succ: a => {
    const $0 = toCharCode(a) + 1 | 0;
    if ($0 >= 0 && $0 <= 65535) { return Data$dMaybe.$Maybe("Just", fromCharCode($0)); }
    return Data$dMaybe.Nothing;
  },
  pred: a => {
    const $0 = toCharCode(a) - 1 | 0;
    if ($0 >= 0 && $0 <= 65535) { return Data$dMaybe.$Maybe("Just", fromCharCode($0)); }
    return Data$dMaybe.Nothing;
  },
  Ord0: () => Data$dOrd.ordChar
};
const cardinality = dict => dict.cardinality;
const boundedEnumUnit = {
  cardinality: 1,
  toEnum: v => {
    if (v === 0) { return Data$dMaybe.$Maybe("Just", undefined); }
    return Data$dMaybe.Nothing;
  },
  fromEnum: v => 0,
  Bounded0: () => Data$dBounded.boundedUnit,
  Enum1: () => enumUnit
};
const boundedEnumOrdering = {
  cardinality: 3,
  toEnum: v => {
    if (v === 0) { return Data$dMaybe.$Maybe("Just", Data$dOrdering.LT); }
    if (v === 1) { return Data$dMaybe.$Maybe("Just", Data$dOrdering.EQ); }
    if (v === 2) { return Data$dMaybe.$Maybe("Just", Data$dOrdering.GT); }
    return Data$dMaybe.Nothing;
  },
  fromEnum: v => {
    if (v === "LT") { return 0; }
    if (v === "EQ") { return 1; }
    if (v === "GT") { return 2; }
    $runtime.fail();
  },
  Bounded0: () => Data$dBounded.boundedOrdering,
  Enum1: () => enumOrdering
};
const boundedEnumChar = {cardinality: 65535, toEnum: charToEnum, fromEnum: toCharCode, Bounded0: () => Data$dBounded.boundedChar, Enum1: () => enumChar};
const boundedEnumBoolean = {
  cardinality: 2,
  toEnum: v => {
    if (v === 0) { return Data$dMaybe.$Maybe("Just", false); }
    if (v === 1) { return Data$dMaybe.$Maybe("Just", true); }
    return Data$dMaybe.Nothing;
  },
  fromEnum: v => {
    if (!v) { return 0; }
    if (v) { return 1; }
    $runtime.fail();
  },
  Bounded0: () => Data$dBounded.boundedBoolean,
  Enum1: () => enumBoolean
};
;
export * from "./foreign.js";
