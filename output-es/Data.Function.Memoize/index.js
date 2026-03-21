// | This module defines functions for _memoizing_ functions, i.e. creating functions which
// | remember their results.
// |
// | This module works by turning a function into a lazily-evaluated data structure depending on
// | its domain type.
import * as $runtime from "../runtime.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dEnum from "../Data.Enum/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dGeneric$dRep from "../Data.Generic.Rep/index.js";
import * as Data$dLazy from "../Data.Lazy/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dString$dCodeUnits from "../Data.String.CodeUnits/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Data$dUnfoldable from "../Data.Unfoldable/index.js";
const $NatTrie = (_1, _2, _3) => ({tag: "NatTrie", _1, _2, _3});
const toUnfoldable = /* #__PURE__ */ (() => Data$dUnfoldable.unfoldableArray.unfoldr(xs => {
  if (xs.tag === "Nil") { return Data$dMaybe.Nothing; }
  if (xs.tag === "Cons") { return Data$dMaybe.$Maybe("Just", Data$dTuple.$Tuple(xs._1, xs._2)); }
  $runtime.fail();
}))();
const fromFoldable = /* #__PURE__ */ Data$dFoldable.foldrArray(Data$dList$dTypes.Cons)(Data$dList$dTypes.Nil);
const NatTrie = value0 => value1 => value2 => $NatTrie(value0, value1, value2);
const tabulateUnit = {
  tabulate: f => {
    const r = Data$dLazy.defer(v => f());
    return v => r;
  }
};
const tabulateNoArguments = {
  tabulate: f => {
    const r = Data$dLazy.defer(v => f(Data$dGeneric$dRep.NoArguments));
    return v => r;
  }
};
const tabulateNat = {
  tabulate: f => {
    const walk = v => v1 => {
      if (v.tag === "Nil") { return v1._1; }
      if (v.tag === "Cons") {
        if (!v._1) {
          const $0 = v1._2;
          const $1 = walk(v._2);
          return Data$dLazy.defer(v$1 => Data$dLazy.force($1(Data$dLazy.force($0))));
        }
        if (v._1) {
          const $0 = v1._3;
          const $1 = walk(v._2);
          return Data$dLazy.defer(v$1 => Data$dLazy.force($1(Data$dLazy.force($0))));
        }
      }
      $runtime.fail();
    };
    const build = n => $NatTrie(Data$dLazy.defer(v => f(n)), Data$dLazy.defer(v => build(n * 2 | 0)), Data$dLazy.defer(v => build((n * 2 | 0) + 1 | 0)));
    const trie = build(0);
    const bits$p = bits$p$a0$copy => bits$p$a1$copy => {
      let bits$p$a0 = bits$p$a0$copy, bits$p$a1 = bits$p$a1$copy, bits$p$c = true, bits$p$r;
      while (bits$p$c) {
        const v = bits$p$a0, v1 = bits$p$a1;
        if (v1 === 0) {
          bits$p$c = false;
          bits$p$r = v;
          continue;
        }
        bits$p$a0 = Data$dList$dTypes.$List("Cons", (v1 & 1) !== 0, v);
        bits$p$a1 = v1 >>> 1;
      }
      return bits$p$r;
    };
    const bits = bits$p(Data$dList$dTypes.Nil);
    return n => walk(bits(n))(trie);
  }
};
const tabulateBool = {
  tabulate: f => {
    const r2 = Data$dLazy.defer(v => f(false));
    const r1 = Data$dLazy.defer(v => f(true));
    return b => {
      if (b) { return r1; }
      return r2;
    };
  }
};
const tabulate = dict => dict.tabulate;
const tabulateArgument = dictTabulate => ({tabulate: f => dictTabulate.tabulate(x => f(x))});
const tabulateChar = {
  tabulate: f => {
    const $0 = tabulateNat.tabulate(x => f((() => {
      if (x >= 0 && x <= 65535) { return Data$dEnum.fromCharCode(x); }
      $runtime.fail();
    })()));
    return x => $0(Data$dEnum.toCharCode(x));
  }
};
const tabulateConstructor = dictTabulate => ({tabulate: f => dictTabulate.tabulate(x => f(x))});
const tabulateEither = dictTabulate => dictTabulate1 => (
  {
    tabulate: f => {
      const r = dictTabulate1.tabulate(x => f(Data$dEither.$Either("Right", x)));
      const l = dictTabulate.tabulate(x => f(Data$dEither.$Either("Left", x)));
      return v => {
        if (v.tag === "Left") { return l(v._1); }
        if (v.tag === "Right") { return r(v._1); }
        $runtime.fail();
      };
    }
  }
);
const tabulateMaybe = dictTabulate => (
  {
    tabulate: f => {
      const n = Data$dLazy.defer(v => f(Data$dMaybe.Nothing));
      const j = dictTabulate.tabulate(x => f(Data$dMaybe.$Maybe("Just", x)));
      return v => {
        if (v.tag === "Nothing") { return n; }
        if (v.tag === "Just") { return j(v._1); }
        $runtime.fail();
      };
    }
  }
);
const tabulateProduct = dictTabulate => dictTabulate1 => (
  {
    tabulate: f => {
      const f$p = dictTabulate.tabulate(a => dictTabulate1.tabulate(b => f(Data$dGeneric$dRep.$Product(a, b))));
      return v => {
        const $0 = v._2;
        const $1 = f$p(v._1);
        return Data$dLazy.defer(v$1 => Data$dLazy.force(Data$dLazy.force($1)($0)));
      };
    }
  }
);
const tabulateSum = dictTabulate => dictTabulate1 => (
  {
    tabulate: f => {
      const r = dictTabulate1.tabulate(x => f(Data$dGeneric$dRep.$Sum("Inr", x)));
      const l = dictTabulate.tabulate(x => f(Data$dGeneric$dRep.$Sum("Inl", x)));
      return v => {
        if (v.tag === "Inl") { return l(v._1); }
        if (v.tag === "Inr") { return r(v._1); }
        $runtime.fail();
      };
    }
  }
);
const tabulateTuple = dictTabulate => dictTabulate1 => (
  {
    tabulate: f => {
      const f$p = dictTabulate.tabulate(a => dictTabulate1.tabulate(b => f(Data$dTuple.$Tuple(a, b))));
      return v => {
        const $0 = v._2;
        const $1 = f$p(v._1);
        return Data$dLazy.defer(v$1 => Data$dLazy.force(Data$dLazy.force($1)($0)));
      };
    }
  }
);
const tabulateList = dictTabulate => (
  {
    tabulate: f => {
      const $0 = tabulateMaybe(tabulateTuple(dictTabulate)(tabulateList(dictTabulate))).tabulate(x => f((() => {
        if (x.tag === "Nothing") { return Data$dList$dTypes.Nil; }
        if (x.tag === "Just") { return Data$dList$dTypes.$List("Cons", x._1._1, x._1._2); }
        $runtime.fail();
      })()));
      return x => $0((() => {
        if (x.tag === "Nil") { return Data$dMaybe.Nothing; }
        if (x.tag === "Cons") { return Data$dMaybe.$Maybe("Just", Data$dTuple.$Tuple(x._1, x._2)); }
        $runtime.fail();
      })());
    }
  }
);
const tabulateArray = dictTabulate => (
  {
    tabulate: f => {
      const $0 = tabulateList(dictTabulate).tabulate(x => f(toUnfoldable(x)));
      return x => $0(fromFoldable(x));
    }
  }
);
const tabulate2 = /* #__PURE__ */ (() => tabulateArray(tabulateChar).tabulate)();
const tabulateString = {
  tabulate: f => {
    const $0 = tabulate2(x => f(Data$dString$dCodeUnits.fromCharArray(x)));
    return x => $0(Data$dString$dCodeUnits.toCharArray(x));
  }
};
const memoize = dictTabulate => f => {
  const $0 = dictTabulate.tabulate(f);
  return x => Data$dLazy.force($0(x));
};
const memoize2 = dictTabulate => dictTabulate1 => {
  const memoize1 = memoize(tabulateTuple(dictTabulate)(dictTabulate1));
  return f => {
    const $0 = memoize1(v => f(v._1)(v._2));
    return a => b => $0(Data$dTuple.$Tuple(a, b));
  };
};
const memoize3 = dictTabulate => dictTabulate1 => {
  const tabulateTuple2 = tabulateTuple(tabulateTuple(dictTabulate)(dictTabulate1));
  return dictTabulate2 => {
    const memoize1 = memoize(tabulateTuple2(dictTabulate2));
    return f => {
      const $0 = memoize1(v => f(v._1._1)(v._1._2)(v._2));
      return a => b => b$1 => $0(Data$dTuple.$Tuple(Data$dTuple.$Tuple(a, b), b$1));
    };
  };
};
const genericTabulate = dictGeneric => dictTabulate => f => {
  const $0 = dictTabulate.tabulate(x => f(dictGeneric.to(x)));
  return x => $0(dictGeneric.from(x));
};
export {
  $NatTrie,
  NatTrie,
  fromFoldable,
  genericTabulate,
  memoize,
  memoize2,
  memoize3,
  tabulate,
  tabulate2,
  tabulateArgument,
  tabulateArray,
  tabulateBool,
  tabulateChar,
  tabulateConstructor,
  tabulateEither,
  tabulateList,
  tabulateMaybe,
  tabulateNat,
  tabulateNoArguments,
  tabulateProduct,
  tabulateString,
  tabulateSum,
  tabulateTuple,
  tabulateUnit,
  toUnfoldable
};
