import * as $runtime from "../runtime.js";
import * as Data$dArray from "../Data.Array/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dHeytingAlgebra from "../Data.HeytingAlgebra/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dMap$dInternal from "../Data.Map.Internal/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Dict from "../Dict/index.js";
import * as Effect$dException from "../Effect.Exception/index.js";
import * as Foreign$dObject from "../Foreign.Object/index.js";
import * as Util from "../Util/index.js";
import * as Util$dMap from "../Util.Map/index.js";
import * as Util$dPair from "../Util.Pair/index.js";
const identity = x => x;
const length = /* #__PURE__ */ Data$dFoldable.foldlArray(c => v => 1 + c | 0)(0);
const negUnit = {neg: identity};
const negBoolean = {neg: Data$dHeytingAlgebra.boolNot};
const meetSemilatticeUnit = {meet: v => identity};
const meetSemilatticeBoolean = {meet: Data$dHeytingAlgebra.boolConj};
const joinSemilatticeUnit = {join: v => identity};
const joinSemilatticeBoolean = {join: Data$dHeytingAlgebra.boolDisj};
const boundedMeetSemilatticeUni = {top: undefined, MeetSemilattice0: () => meetSemilatticeUnit};
const boundedMeetSemilatticeBoo = {top: true, MeetSemilattice0: () => meetSemilatticeBoolean};
const boundedLattice = dictBoundedJoinSemilattice => dictBoundedMeetSemilattice => (
  {BoundedJoinSemilattice0: () => dictBoundedJoinSemilattice, BoundedMeetSemilattice1: () => dictBoundedMeetSemilattice}
);
const boundedJoinSemilatticeUni = {bot: undefined, JoinSemilattice0: () => joinSemilatticeUnit};
const boundedJoinSemilatticeBoo = {bot: false, JoinSemilattice0: () => joinSemilatticeBoolean};
const boundedLattice1 = {BoundedJoinSemilattice0: () => boundedJoinSemilatticeBoo, BoundedMeetSemilattice1: () => boundedMeetSemilatticeBoo};
const booleanLatticeBoolean = {BoundedLattice0: () => boundedLattice1, Neg1: () => negBoolean};
const booleanLattice = dictBoundedLattice => dictNeg => ({BoundedLattice0: () => dictBoundedLattice, Neg1: () => dictNeg});
const topOf = dict => dict.topOf;
const topOf1 = dictFunctor => dictTopOf => ({topOf: dictFunctor.map(dictTopOf.topOf)});
const topOf$x215$x215 = dictTopOf => dictTopOf1 => ({topOf: x => Data$dTuple.$Tuple(dictTopOf.topOf(x._1), dictTopOf1.topOf(x._2))});
const top = dict => dict.top;
const topOf2 = dictFunctor => dictBoundedMeetSemilattice => (
  {
    topOf: dictFunctor.map((() => {
      const $0 = dictBoundedMeetSemilattice.top;
      return v => $0;
    })())
  }
);
const topOfUnit$x215Raw$x215 = dictFunctor => dictBoundedMeetSemilattice => {
  const top1 = dictBoundedMeetSemilattice.top;
  return {
    topOf: (() => {
      const $0 = dictFunctor.map(v => top1);
      return x => Data$dTuple.$Tuple(top1, $0(x._2));
    })()
  };
};
const neg = dict => dict.neg;
const neg1 = dictFunctor => dictNeg => {
  const neg2 = dictNeg.neg;
  return {neg: x => dictFunctor.map(neg2)(x)};
};
const neg$x215 = dictNeg => dictNeg1 => ({neg: x => Data$dTuple.$Tuple(dictNeg.neg(x._1), dictNeg1.neg(x._2))});
const meet = dict => dict.meet;
const relativeComplement = dictNeg => dictMeetSemilattice => a => x => dictMeetSemilattice.meet(dictNeg.neg(x))(a);
const symmetricDiff = dictNeg => dictMeetSemilattice => x => y => Data$dTuple.$Tuple(dictMeetSemilattice.meet(dictNeg.neg(y))(x), dictMeetSemilattice.meet(dictNeg.neg(x))(y));
const meetSemilatticeDict = dictMeetSemilattice => ({meet: Dict.mapDictString.unionWith(dictMeetSemilattice.meet)});
const meetSemilattice$x215 = dictMeetSemilattice => dictMeetSemilattice1 => (
  {meet: v => v1 => Data$dTuple.$Tuple(dictMeetSemilattice.meet(v._1)(v1._1), dictMeetSemilattice1.meet(v._2)(v1._2))}
);
const boundedMeetSemilattice$x215 = dictBoundedMeetSemilattice => {
  const top1 = dictBoundedMeetSemilattice.top;
  const $0 = dictBoundedMeetSemilattice.MeetSemilattice0();
  return dictBoundedMeetSemilattice1 => {
    const $1 = dictBoundedMeetSemilattice1.MeetSemilattice0();
    const meetSemilattice$x2152 = {meet: v => v1 => Data$dTuple.$Tuple($0.meet(v._1)(v1._1), $1.meet(v._2)(v1._2))};
    return {top: Data$dTuple.$Tuple(top1, dictBoundedMeetSemilattice1.top), MeetSemilattice0: () => meetSemilattice$x2152};
  };
};
const join = dict => dict.join;
const joinSemilatticeArray = dictJoinSemilattice => {
  const join1 = dictJoinSemilattice.join;
  return {
    join: xs => ys => {
      if (length(xs) === length(ys)) { return Data$dArray.zipWithImpl(join1, xs, ys); }
      return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
    }
  };
};
const joinSemilatticeDict = dictJoinSemilattice => ({join: Dict.mapDictString.unionWith(dictJoinSemilattice.join)});
const joinSemilatticeList = dictJoinSemilattice => (
  {
    join: xs => ys => {
      if (
        (() => {
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
                go$a0 = 1 + b | 0;
                go$a1 = v._2;
                continue;
              }
              $runtime.fail();
            }
            return go$r;
          };
          const go$1 = go$1$a0$copy => go$1$a1$copy => {
            let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
            while (go$1$c) {
              const b = go$1$a0, v = go$1$a1;
              if (v.tag === "Nil") {
                go$1$c = false;
                go$1$r = b;
                continue;
              }
              if (v.tag === "Cons") {
                go$1$a0 = 1 + b | 0;
                go$1$a1 = v._2;
                continue;
              }
              $runtime.fail();
            }
            return go$1$r;
          };
          return go(0)(xs) === go$1(0)(ys);
        })()
      ) {
        const go = go$a0$copy => go$a1$copy => go$a2$copy => {
          let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$a2 = go$a2$copy, go$c = true, go$r;
          while (go$c) {
            const v = go$a0, v1 = go$a1, v2 = go$a2;
            if (v.tag === "Nil") {
              go$c = false;
              go$r = v2;
              continue;
            }
            if (v1.tag === "Nil") {
              go$c = false;
              go$r = v2;
              continue;
            }
            if (v.tag === "Cons" && v1.tag === "Cons") {
              go$a0 = v._2;
              go$a1 = v1._2;
              go$a2 = Data$dList$dTypes.$List("Cons", dictJoinSemilattice.join(v._1)(v1._1), v2);
              continue;
            }
            $runtime.fail();
          }
          return go$r;
        };
        const go$1 = go$1$a0$copy => go$1$a1$copy => {
          let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
          while (go$1$c) {
            const v = go$1$a0, v1 = go$1$a1;
            if (v1.tag === "Nil") {
              go$1$c = false;
              go$1$r = v;
              continue;
            }
            if (v1.tag === "Cons") {
              go$1$a0 = Data$dList$dTypes.$List("Cons", v1._1, v);
              go$1$a1 = v1._2;
              continue;
            }
            $runtime.fail();
          }
          return go$1$r;
        };
        return go$1(Data$dList$dTypes.Nil)(go(xs)(ys)(Data$dList$dTypes.Nil));
      }
      return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
    }
  }
);
const joinSemilatticeMaybe = dictJoinSemilattice => (
  {
    join: v => v1 => {
      if (v.tag === "Nothing") {
        if (v1.tag === "Nothing") { return Data$dMaybe.Nothing; }
        return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
      }
      if (v.tag === "Just" && v1.tag === "Just") { return Data$dMaybe.$Maybe("Just", dictJoinSemilattice.join(v._1)(v1._1)); }
      return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
    }
  }
);
const joinSemilatticePair = dictJoinSemilattice => ({join: v => v1 => Util$dPair.$Pair(dictJoinSemilattice.join(v._1)(v1._1), dictJoinSemilattice.join(v._2)(v1._2))});
const joinSemilattice$x215 = dictJoinSemilattice => dictJoinSemilattice1 => (
  {join: v => v1 => Data$dTuple.$Tuple(dictJoinSemilattice.join(v._1)(v1._1), dictJoinSemilattice1.join(v._2)(v1._2))}
);
const expand = dict => dict.expand;
const expandableArrayArray = dictExpandable => {
  const expand1 = dictExpandable.expand;
  return {expand: xs => Data$dArray.zipWith(expand1)(xs)};
};
const expandableListList = dictExpandable => (
  {
    expand: xs => ys => {
      const go = go$a0$copy => go$a1$copy => go$a2$copy => {
        let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$a2 = go$a2$copy, go$c = true, go$r;
        while (go$c) {
          const v = go$a0, v1 = go$a1, v2 = go$a2;
          if (v.tag === "Nil") {
            go$c = false;
            go$r = v2;
            continue;
          }
          if (v1.tag === "Nil") {
            go$c = false;
            go$r = v2;
            continue;
          }
          if (v.tag === "Cons" && v1.tag === "Cons") {
            go$a0 = v._2;
            go$a1 = v1._2;
            go$a2 = Data$dList$dTypes.$List("Cons", dictExpandable.expand(v._1)(v1._1), v2);
            continue;
          }
          $runtime.fail();
        }
        return go$r;
      };
      const go$1 = go$1$a0$copy => go$1$a1$copy => {
        let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
        while (go$1$c) {
          const v = go$1$a0, v1 = go$1$a1;
          if (v1.tag === "Nil") {
            go$1$c = false;
            go$1$r = v;
            continue;
          }
          if (v1.tag === "Cons") {
            go$1$a0 = Data$dList$dTypes.$List("Cons", v1._1, v);
            go$1$a1 = v1._2;
            continue;
          }
          $runtime.fail();
        }
        return go$1$r;
      };
      return go$1(Data$dList$dTypes.Nil)(go(xs)(ys)(Data$dList$dTypes.Nil));
    }
  }
);
const expandableMaybeMaybe = dictExpandable => (
  {
    expand: v => v1 => {
      if (v.tag === "Nothing") {
        if (v1.tag === "Nothing") { return Data$dMaybe.Nothing; }
        return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
      }
      if (v.tag === "Just" && v1.tag === "Just") { return Data$dMaybe.$Maybe("Just", dictExpandable.expand(v._1)(v1._1)); }
      return Effect$dException.throwException(Effect$dException.error("Shape mismatch"))();
    }
  }
);
const expandablePairPair = dictExpandable => ({expand: v => v1 => Util$dPair.$Pair(dictExpandable.expand(v._1)(v1._1), dictExpandable.expand(v._2)(v1._2))});
const expandable$x215Unit$x215Raw = dictExpandable => ({expand: v => v1 => Data$dTuple.$Tuple(v._1, dictExpandable.expand(v._2)(v1._2))});
const erase = dictFunctor => dictFunctor.map(v => {});
const botOf = dict => dict.botOf;
const botOf1 = dictFunctor => dictBotOf => ({botOf: dictFunctor.map(dictBotOf.botOf)});
const botOf$x215$x215 = dictBotOf => dictBotOf1 => ({botOf: x => Data$dTuple.$Tuple(dictBotOf.botOf(x._1), dictBotOf1.botOf(x._2))});
const expandableDictDict = dictBotOf => {
  const botOf3 = dictBotOf.botOf;
  return dictExpandable => {
    const expand1 = dictExpandable.expand;
    return {
      expand: kvs => kvs$p => Util.assertWith("")(Data$dMap$dInternal.unsafeDifference(
        Data$dOrd.ordString.compare,
        Util$dMap.mapObjectString.keys(kvs),
        Util$dMap.mapObjectString.keys(kvs$p)
      ).tag === "Leaf")(Foreign$dObject.union(Util$dMap.intersectionWith_Object(expand1)(kvs)(kvs$p))(Foreign$dObject._fmapObject(
        Util$dMap.mapFObjectString.difference(kvs$p)(kvs),
        botOf3
      )))
    };
  };
};
const bot = dict => dict.bot;
const botOf2 = dictFunctor => dictBoundedJoinSemilattice => (
  {
    botOf: dictFunctor.map((() => {
      const $0 = dictBoundedJoinSemilattice.bot;
      return v => $0;
    })())
  }
);
const botOfUnit$x215Raw$x215 = dictFunctor => dictBoundedJoinSemilattice => (
  {
    botOf: (() => {
      const $0 = dictBoundedJoinSemilattice.bot;
      const $1 = dictFunctor.map((() => {
        const $1 = dictBoundedJoinSemilattice.bot;
        return v => $1;
      })());
      return x => Data$dTuple.$Tuple($0, $1(x._2));
    })()
  }
);
const boundedJoinSemilattice$x215 = dictBoundedJoinSemilattice => {
  const bot1 = dictBoundedJoinSemilattice.bot;
  const $0 = dictBoundedJoinSemilattice.JoinSemilattice0();
  return dictBoundedJoinSemilattice1 => {
    const $1 = dictBoundedJoinSemilattice1.JoinSemilattice0();
    const joinSemilattice$x2152 = {join: v => v1 => Data$dTuple.$Tuple($0.join(v._1)(v1._1), $1.join(v._2)(v1._2))};
    return {bot: Data$dTuple.$Tuple(bot1, dictBoundedJoinSemilattice1.bot), JoinSemilattice0: () => joinSemilattice$x2152};
  };
};
const booleanLattice$x215 = dictBooleanLattice => {
  const BoundedLattice0 = dictBooleanLattice.BoundedLattice0();
  const $0 = BoundedLattice0.BoundedJoinSemilattice0();
  const bot1 = $0.bot;
  const $1 = $0.JoinSemilattice0();
  const $2 = BoundedLattice0.BoundedMeetSemilattice1();
  const top1 = $2.top;
  const $3 = $2.MeetSemilattice0();
  const $4 = dictBooleanLattice.Neg1();
  return dictBooleanLattice1 => {
    const BoundedLattice01 = dictBooleanLattice1.BoundedLattice0();
    const $5 = BoundedLattice01.BoundedJoinSemilattice0();
    const $6 = $5.JoinSemilattice0();
    const $7 = (() => {
      const joinSemilattice$x2152 = {join: v => v1 => Data$dTuple.$Tuple($1.join(v._1)(v1._1), $6.join(v._2)(v1._2))};
      return {bot: Data$dTuple.$Tuple(bot1, $5.bot), JoinSemilattice0: () => joinSemilattice$x2152};
    })();
    const $8 = BoundedLattice01.BoundedMeetSemilattice1();
    const $9 = $8.MeetSemilattice0();
    const $10 = (() => {
      const meetSemilattice$x2152 = {meet: v => v1 => Data$dTuple.$Tuple($3.meet(v._1)(v1._1), $9.meet(v._2)(v1._2))};
      return {top: Data$dTuple.$Tuple(top1, $8.top), MeetSemilattice0: () => meetSemilattice$x2152};
    })();
    const $11 = dictBooleanLattice1.Neg1();
    const neg$x2152 = {neg: x => Data$dTuple.$Tuple($4.neg(x._1), $11.neg(x._2))};
    return {BoundedLattice0: () => ({BoundedJoinSemilattice0: () => $7, BoundedMeetSemilattice1: () => $10}), Neg1: () => neg$x2152};
  };
};
export {
  
  
  
  
  
  
  
  
  
  
  boundedJoinSemilatticeBoo,
  boundedJoinSemilatticeUni,
  
  
  
  boundedMeetSemilatticeBoo,
  boundedMeetSemilatticeUni,
  
  
  
  
  expandableDictDict,
  
  expandableMaybeMaybe,
  
  
  
  
  joinSemilatticeArray,
  joinSemilatticeBoolean,
  
  joinSemilatticeList,
  joinSemilatticeMaybe,
  
  joinSemilatticeUnit,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
};
