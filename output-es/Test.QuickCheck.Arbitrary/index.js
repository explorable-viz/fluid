import * as $runtime from "../runtime.js";
import * as Control$dMonad$dGen$dCommon from "../Control.Monad.Gen.Common/index.js";
import * as Data$dArray$dST from "../Data.Array.ST/index.js";
import * as Data$dEnum from "../Data.Enum/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dGeneric$dRep from "../Data.Generic.Rep/index.js";
import * as Data$dInt from "../Data.Int/index.js";
import * as Data$dLazy from "../Data.Lazy/index.js";
import * as Data$dNonEmpty from "../Data.NonEmpty/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
import * as Data$dString$dCodeUnits from "../Data.String.CodeUnits/index.js";
import * as Data$dString$dCommon from "../Data.String.Common/index.js";
import * as Data$dString$dNonEmpty$dCodeUnits from "../Data.String.NonEmpty.CodeUnits/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Record$dUnsafe from "../Record.Unsafe/index.js";
import * as Test$dQuickCheck$dGen from "../Test.QuickCheck.Gen/index.js";
import * as Type$dProxy from "../Type.Proxy/index.js";
const identity = x => x;
const genMaybe = /* #__PURE__ */ Control$dMonad$dGen$dCommon.genMaybe$p(Test$dQuickCheck$dGen.monadGenGen)(0.75);
const genEither = /* #__PURE__ */ Control$dMonad$dGen$dCommon.genEither$p(Test$dQuickCheck$dGen.monadGenGen)(0.5);
const coarbitraryNoArguments = {coarbitrary: v => identity};
const coarbitrary = dict => dict.coarbitrary;
const coarbitraryArgument = dictCoarbitrary => ({coarbitrary: v => dictCoarbitrary.coarbitrary(v)});
const coarbitraryConstructor = dictCoarbitrary => ({coarbitrary: v => dictCoarbitrary.coarbitrary(v)});
const coarbitraryProduct = dictCoarbitrary => dictCoarbitrary1 => (
  {
    coarbitrary: v => {
      const $0 = dictCoarbitrary.coarbitrary(v._1);
      const $1 = dictCoarbitrary1.coarbitrary(v._2);
      return x => $1($0(x));
    }
  }
);
const coarbitrarySum = dictCoarbitrary => dictCoarbitrary1 => (
  {
    coarbitrary: v => {
      if (v.tag === "Inl") { return dictCoarbitrary.coarbitrary(v._1); }
      if (v.tag === "Inr") { return dictCoarbitrary1.coarbitrary(v._1); }
      $runtime.fail();
    }
  }
);
const genericCoarbitrary = dictGeneric => dictCoarbitrary => x => dictCoarbitrary.coarbitrary(dictGeneric.from(x));
const coarbUnit = {coarbitrary: v => Test$dQuickCheck$dGen.perturbGen(1.0)};
const coarbTuple = dictCoarbitrary => dictCoarbitrary1 => (
  {
    coarbitrary: v => {
      const $0 = dictCoarbitrary.coarbitrary(v._1);
      const $1 = dictCoarbitrary1.coarbitrary(v._2);
      return x => $1($0(x));
    }
  }
);
const coarbOrdering = {
  coarbitrary: v => {
    if (v === "LT") { return Test$dQuickCheck$dGen.perturbGen(1.0); }
    if (v === "EQ") { return Test$dQuickCheck$dGen.perturbGen(2.0); }
    if (v === "GT") { return Test$dQuickCheck$dGen.perturbGen(3.0); }
    $runtime.fail();
  }
};
const coarbNumber = {coarbitrary: Test$dQuickCheck$dGen.perturbGen};
const coarbNonEmpty = dictCoarbitrary => dictCoarbitrary1 => (
  {
    coarbitrary: v => {
      const $0 = dictCoarbitrary1.coarbitrary(v._1);
      const $1 = dictCoarbitrary.coarbitrary(v._2);
      return x => $1($0(x));
    }
  }
);
const coarbMaybe = dictCoarbitrary => (
  {
    coarbitrary: v => {
      if (v.tag === "Nothing") { return Test$dQuickCheck$dGen.perturbGen(1.0); }
      if (v.tag === "Just") { return dictCoarbitrary.coarbitrary(v._1); }
      $runtime.fail();
    }
  }
);
const coarbList = dictCoarbitrary => (
  {
    coarbitrary: (() => {
      const go = go$a0$copy => go$a1$copy => {
        let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
        while (go$c) {
          const b = go$a0, v = go$a1;
          if (v.tag === "Nil") {
            go$c = false;
            go$r = b;
            continue;
          }
          if (v.tag === "Cons") {
            go$a0 = (() => {
              const $0 = dictCoarbitrary.coarbitrary(v._1);
              return x => b($0(x));
            })();
            go$a1 = v._2;
            continue;
          }
          $runtime.fail();
        }
        return go$r;
      };
      return go(identity);
    })()
  }
);
const coarbNonEmptyList = dictCoarbitrary => {
  const $0 = coarbList(dictCoarbitrary);
  return {
    coarbitrary: v => {
      const $1 = dictCoarbitrary.coarbitrary(v._1);
      const $2 = $0.coarbitrary(v._2);
      return x => $2($1(x));
    }
  };
};
const coarbLazy = dictCoarbitrary => ({coarbitrary: a => dictCoarbitrary.coarbitrary(Data$dLazy.force(a))});
const coarbInt = {coarbitrary: x => Test$dQuickCheck$dGen.perturbGen(Data$dInt.toNumber(x))};
const coarbIdentity = dictCoarbitrary => ({coarbitrary: v => dictCoarbitrary.coarbitrary(v)});
const coarbEither = dictCoarbitrary => dictCoarbitrary1 => (
  {
    coarbitrary: v => {
      if (v.tag === "Left") { return dictCoarbitrary.coarbitrary(v._1); }
      if (v.tag === "Right") { return dictCoarbitrary1.coarbitrary(v._1); }
      $runtime.fail();
    }
  }
);
const coarbChar = {coarbitrary: c => coarbInt.coarbitrary(Data$dEnum.toCharCode(c))};
const coarbBoolean = {
  coarbitrary: v => {
    if (v) { return Test$dQuickCheck$dGen.perturbGen(1.0); }
    return Test$dQuickCheck$dGen.perturbGen(2.0);
  }
};
const coarbArray = dictCoarbitrary => (
  {
    coarbitrary: Data$dFoldable.foldlArray(f => x => {
      const $0 = dictCoarbitrary.coarbitrary(x);
      return x$1 => f($0(x$1));
    })(identity)
  }
);
const coarbitrary2 = /* #__PURE__ */ (() => coarbArray(coarbMaybe(coarbChar)).coarbitrary)();
const coarbNonEmptyArray = dictCoarbitrary => ({coarbitrary: x => coarbArray(dictCoarbitrary).coarbitrary(x)});
const coarbString = {coarbitrary: s => coarbitrary2(Data$dFunctor.arrayMap(Data$dString$dCodeUnits.charAt(0))(Data$dString$dCommon.split("")(s)))};
const coarbNonEmptyString = {coarbitrary: x => coarbString.coarbitrary(x)};
const arbitraryRowListNil = {arbitraryRecord: v => Test$dQuickCheck$dGen.applicativeGen.pure({})};
const arbitraryRecord = dict => dict.arbitraryRecord;
const arbitraryRecordInstance = () => dictArbitraryRowList => ({arbitrary: dictArbitraryRowList.arbitraryRecord(Type$dProxy.Proxy)});
const arbitraryNoArguments = /* #__PURE__ */ (() => ({arbitrary: Test$dQuickCheck$dGen.applicativeGen.pure(Data$dGeneric$dRep.NoArguments)}))();
const arbitraryGenericSum = dict => dict.arbitraryGenericSum;
const arbitrary = dict => dict.arbitrary;
const arbitraryArgument = dictArbitrary => (
  {
    arbitrary: s => {
      const $0 = dictArbitrary.arbitrary(s);
      return Data$dTuple.$Tuple($0._1, $0._2);
    }
  }
);
const arbitraryConstructor = dictArbitrary => (
  {
    arbitrary: s => {
      const $0 = dictArbitrary.arbitrary(s);
      return Data$dTuple.$Tuple($0._1, $0._2);
    }
  }
);
const arbitraryIdentity = dictArbitrary => (
  {
    arbitrary: s => {
      const $0 = dictArbitrary.arbitrary(s);
      return Data$dTuple.$Tuple($0._1, $0._2);
    }
  }
);
const arbitraryLazy = dictArbitrary => (
  {arbitrary: Test$dQuickCheck$dGen.bindStateT.bind(dictArbitrary.arbitrary)(x => Test$dQuickCheck$dGen.applicativeGen.pure(Data$dLazy.defer(v => x)))}
);
const arbitraryList = dictArbitrary => {
  const arbitrary1 = dictArbitrary.arbitrary;
  return {
    arbitrary: Test$dQuickCheck$dGen.monadStateStateT.state(s => Test$dQuickCheck$dGen.bindStateT.bind(0 <= s.size
      ? Test$dQuickCheck$dGen.chooseInt$p(0)(s.size)
      : Test$dQuickCheck$dGen.chooseInt$p(s.size)(0))(a => Test$dQuickCheck$dGen.listOf(a)(arbitrary1))(s))
  };
};
const arbitraryProduct = dictArbitrary => dictArbitrary1 => (
  {
    arbitrary: Test$dQuickCheck$dGen.applyGen.apply(s => {
      const $0 = dictArbitrary.arbitrary(s);
      return Data$dTuple.$Tuple(Data$dGeneric$dRep.Product($0._1), $0._2);
    })(dictArbitrary1.arbitrary)
  }
);
const arbitraryRowListCons = dictArbitrary => {
  const arbitrary1 = dictArbitrary.arbitrary;
  return dictArbitraryRowList => () => () => () => dictIsSymbol => (
    {
      arbitraryRecord: v => Test$dQuickCheck$dGen.bindStateT.bind(arbitrary1)(value => Test$dQuickCheck$dGen.bindStateT.bind(dictArbitraryRowList.arbitraryRecord(Type$dProxy.Proxy))(previous => Test$dQuickCheck$dGen.applicativeGen.pure(Record$dUnsafe.unsafeSet(dictIsSymbol.reflectSymbol(Type$dProxy.Proxy))(value)(previous))))
    }
  );
};
const arbitrarySum = dictArbitrary => dictArbitraryGenericSum => (
  {
    arbitrary: Test$dQuickCheck$dGen.oneOf((() => {
      const $0 = [
        s => {
          const $0 = dictArbitrary.arbitrary(s);
          return Data$dTuple.$Tuple(Data$dGeneric$dRep.$Sum("Inl", $0._1), $0._2);
        },
        ...Data$dFunctor.arrayMap(v => s => {
          const $0 = v(s);
          return Data$dTuple.$Tuple(Data$dGeneric$dRep.$Sum("Inr", $0._1), $0._2);
        })(dictArbitraryGenericSum.arbitraryGenericSum)
      ];
      if ($0.length > 0) { return $0; }
      $runtime.fail();
    })())
  }
);
const genericArbitrary = dictGeneric => dictArbitrary => s => {
  const $0 = dictArbitrary.arbitrary(s);
  return Data$dTuple.$Tuple(dictGeneric.to($0._1), $0._2);
};
const arbUnit = /* #__PURE__ */ (() => ({arbitrary: Test$dQuickCheck$dGen.applicativeGen.pure()}))();
const arbTuple = dictArbitrary => dictArbitrary1 => (
  {
    arbitrary: Test$dQuickCheck$dGen.applyGen.apply(s => {
      const $0 = dictArbitrary.arbitrary(s);
      return Data$dTuple.$Tuple(Data$dTuple.Tuple($0._1), $0._2);
    })(dictArbitrary1.arbitrary)
  }
);
const arbOrdering = {arbitrary: /* #__PURE__ */ Test$dQuickCheck$dGen.elements([Data$dOrdering.LT, Data$dOrdering.EQ, Data$dOrdering.GT])};
const arbNumber = {arbitrary: Test$dQuickCheck$dGen.uniform};
const arbNonEmpty = dictArbitrary => {
  const arbitrary1 = dictArbitrary.arbitrary;
  return dictArbitrary1 => (
    {
      arbitrary: Test$dQuickCheck$dGen.applyGen.apply(s => {
        const $0 = dictArbitrary1.arbitrary(s);
        return Data$dTuple.$Tuple(Data$dNonEmpty.NonEmpty($0._1), $0._2);
      })(arbitrary1)
    }
  );
};
const arbNonEmptyList = dictArbitrary => (
  {
    arbitrary: s => {
      const $0 = arbNonEmpty(arbitraryList(dictArbitrary))(dictArbitrary).arbitrary(s);
      return Data$dTuple.$Tuple($0._1, $0._2);
    }
  }
);
const arbMultiplicative = dictArbitrary => (
  {
    arbitrary: s => {
      const $0 = dictArbitrary.arbitrary(s);
      return Data$dTuple.$Tuple($0._1, $0._2);
    }
  }
);
const arbMaybe = dictArbitrary => ({arbitrary: genMaybe(dictArbitrary.arbitrary)});
const arbLast = dictArbitrary => (
  {
    arbitrary: s => {
      const $0 = dictArbitrary.arbitrary(s);
      return Data$dTuple.$Tuple($0._1, $0._2);
    }
  }
);
const arbInt = {arbitrary: /* #__PURE__ */ Test$dQuickCheck$dGen.chooseInt$p(-1000000)(1000000)};
const arbGenSumSum = dictArbitrary => dictArbitraryGenericSum => (
  {
    arbitraryGenericSum: [
      s => {
        const $0 = dictArbitrary.arbitrary(s);
        return Data$dTuple.$Tuple(Data$dGeneric$dRep.$Sum("Inl", $0._1), $0._2);
      },
      ...Data$dFunctor.arrayMap(v => s => {
        const $0 = v(s);
        return Data$dTuple.$Tuple(Data$dGeneric$dRep.$Sum("Inr", $0._1), $0._2);
      })(dictArbitraryGenericSum.arbitraryGenericSum)
    ]
  }
);
const arbGenSumConstructor = dictArbitrary => (
  {
    arbitraryGenericSum: [
      s => {
        const $0 = dictArbitrary.arbitrary(s);
        return Data$dTuple.$Tuple($0._1, $0._2);
      }
    ]
  }
);
const arbFunction = dictCoarbitrary => dictArbitrary => {
  const arbitrary1 = dictArbitrary.arbitrary;
  return {arbitrary: Test$dQuickCheck$dGen.repeatable(a => dictCoarbitrary.coarbitrary(a)(arbitrary1))};
};
const arbFirst = dictArbitrary => (
  {
    arbitrary: s => {
      const $0 = dictArbitrary.arbitrary(s);
      return Data$dTuple.$Tuple($0._1, $0._2);
    }
  }
);
const arbEndo = dictArbitrary => (
  {
    arbitrary: s => {
      const $0 = dictArbitrary.arbitrary(s);
      return Data$dTuple.$Tuple($0._1, $0._2);
    }
  }
);
const arbEither = dictArbitrary => {
  const arbitrary1 = dictArbitrary.arbitrary;
  return dictArbitrary1 => ({arbitrary: genEither(arbitrary1)(dictArbitrary1.arbitrary)});
};
const arbDual = dictArbitrary => (
  {
    arbitrary: s => {
      const $0 = dictArbitrary.arbitrary(s);
      return Data$dTuple.$Tuple($0._1, $0._2);
    }
  }
);
const arbDisj = dictArbitrary => (
  {
    arbitrary: s => {
      const $0 = dictArbitrary.arbitrary(s);
      return Data$dTuple.$Tuple($0._1, $0._2);
    }
  }
);
const arbConj = dictArbitrary => (
  {
    arbitrary: s => {
      const $0 = dictArbitrary.arbitrary(s);
      return Data$dTuple.$Tuple($0._1, $0._2);
    }
  }
);
const arbChar = {
  arbitrary: /* #__PURE__ */ (() => {
    const $0 = Test$dQuickCheck$dGen.chooseInt$p(0)(65536);
    return s => {
      const $1 = $0(s);
      return Data$dTuple.$Tuple(
        (() => {
          if ($1._1 >= 0 && $1._1 <= 65535) { return Data$dEnum.fromCharCode($1._1); }
          if ($1._1 < 0) { return "\u0000"; }
          return "￿";
        })(),
        $1._2
      );
    };
  })()
};
const arbBoolean = /* #__PURE__ */ (() => ({arbitrary: Test$dQuickCheck$dGen.monadGenGen.chooseBool}))();
const arbArray = dictArbitrary => ({arbitrary: Test$dQuickCheck$dGen.arrayOf(dictArbitrary.arbitrary)});
const arbNonEmptyArray = dictArbitrary => {
  const arbitrary1 = Test$dQuickCheck$dGen.arrayOf(dictArbitrary.arbitrary);
  return {
    arbitrary: Test$dQuickCheck$dGen.bindStateT.bind(dictArbitrary.arbitrary)(x => Test$dQuickCheck$dGen.bindStateT.bind(arbitrary1)(xs => Test$dQuickCheck$dGen.applicativeGen.pure((() => {
      const $0 = (() => {
        const mxs = Data$dArray$dST.unsafeThawImpl(xs);
        mxs.push(x);
        return mxs;
      })();
      if ($0.length > 0) { return $0; }
      $runtime.fail();
    })())))
  };
};
const arbString = {
  arbitrary: s => {
    const $0 = Test$dQuickCheck$dGen.arrayOf(arbChar.arbitrary)(s);
    return Data$dTuple.$Tuple(Data$dString$dCodeUnits.fromCharArray($0._1), $0._2);
  }
};
const arbNonEmptyString = /* #__PURE__ */ (() => (
  {
    arbitrary: Test$dQuickCheck$dGen.applyGen.apply(s => {
      const $0 = arbChar.arbitrary(s);
      return Data$dTuple.$Tuple(Data$dString$dNonEmpty$dCodeUnits.cons($0._1), $0._2);
    })(arbString.arbitrary)
  }
))();
const coarbFunction = dictArbitrary => {
  const arbitrary1 = Test$dQuickCheck$dGen.arrayOf(dictArbitrary.arbitrary);
  return dictCoarbitrary => (
    {coarbitrary: f => gen => Test$dQuickCheck$dGen.bindStateT.bind(arbitrary1)(xs => coarbArray(dictCoarbitrary).coarbitrary(Data$dFunctor.arrayMap(f)(xs))(gen))}
  );
};
const arbAdditive = dictArbitrary => (
  {
    arbitrary: s => {
      const $0 = dictArbitrary.arbitrary(s);
      return Data$dTuple.$Tuple($0._1, $0._2);
    }
  }
);
export {
  arbAdditive,
  arbArray,
  arbBoolean,
  arbChar,
  arbConj,
  arbDisj,
  arbDual,
  arbEither,
  arbEndo,
  arbFirst,
  arbFunction,
  arbGenSumConstructor,
  arbGenSumSum,
  arbInt,
  arbLast,
  arbMaybe,
  arbMultiplicative,
  arbNonEmpty,
  arbNonEmptyArray,
  arbNonEmptyList,
  arbNonEmptyString,
  arbNumber,
  arbOrdering,
  arbString,
  arbTuple,
  arbUnit,
  arbitrary,
  arbitraryArgument,
  arbitraryConstructor,
  arbitraryGenericSum,
  arbitraryIdentity,
  arbitraryLazy,
  arbitraryList,
  arbitraryNoArguments,
  arbitraryProduct,
  arbitraryRecord,
  arbitraryRecordInstance,
  arbitraryRowListCons,
  arbitraryRowListNil,
  arbitrarySum,
  coarbArray,
  coarbBoolean,
  coarbChar,
  coarbEither,
  coarbFunction,
  coarbIdentity,
  coarbInt,
  coarbLazy,
  coarbList,
  coarbMaybe,
  coarbNonEmpty,
  coarbNonEmptyArray,
  coarbNonEmptyList,
  coarbNonEmptyString,
  coarbNumber,
  coarbOrdering,
  coarbString,
  coarbTuple,
  coarbUnit,
  coarbitrary,
  coarbitrary2,
  coarbitraryArgument,
  coarbitraryConstructor,
  coarbitraryNoArguments,
  coarbitraryProduct,
  coarbitrarySum,
  genEither,
  genMaybe,
  genericArbitrary,
  genericCoarbitrary,
  identity
};
