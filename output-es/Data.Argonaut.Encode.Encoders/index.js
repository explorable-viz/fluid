import * as $runtime from "../runtime.js";
import * as Data$dArgonaut$dCore from "../Data.Argonaut.Core/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dInt from "../Data.Int/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dMap$dInternal from "../Data.Map.Internal/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dString$dCodePoints from "../Data.String.CodePoints/index.js";
import * as Data$dString$dCodeUnits from "../Data.String.CodeUnits/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Data$dUnfoldable from "../Data.Unfoldable/index.js";
import * as Data$dVoid from "../Data.Void/index.js";
import * as Foreign$dObject from "../Foreign.Object/index.js";
const toUnfoldable = /* #__PURE__ */ (() => Data$dUnfoldable.unfoldableArray.unfoldr(xs => {
  if (xs.tag === "Nil") { return Data$dMaybe.Nothing; }
  if (xs.tag === "Cons") { return Data$dMaybe.$Maybe("Just", Data$dTuple.$Tuple(xs._1, xs._2)); }
  $runtime.fail();
}))();
const toUnfoldable1 = x => {
  const go = go$a0$copy => go$a1$copy => {
    let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
    while (go$c) {
      const source = go$a0, memo = go$a1;
      const v = Data$dMap$dInternal.stepUnfoldr(source);
      if (v.tag === "Nothing") {
        const go$1 = go$1$a0$copy => go$1$a1$copy => {
          let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
          while (go$1$c) {
            const b = go$1$a0, v$1 = go$1$a1;
            if (v$1.tag === "Nil") {
              go$1$c = false;
              go$1$r = b;
              continue;
            }
            if (v$1.tag === "Cons") {
              go$1$a0 = Data$dList$dTypes.$List("Cons", v$1._1, b);
              go$1$a1 = v$1._2;
              continue;
            }
            $runtime.fail();
          }
          return go$1$r;
        };
        go$c = false;
        go$r = go$1(Data$dList$dTypes.Nil)(memo);
        continue;
      }
      if (v.tag === "Just") {
        go$a0 = v._1._2;
        go$a1 = Data$dList$dTypes.$List("Cons", v._1._1, memo);
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  return go(Data$dMap$dInternal.$MapIter("IterNode", x, Data$dMap$dInternal.IterLeaf))(Data$dList$dTypes.Nil);
};
const toUnfoldable2 = x => {
  const go = (m$p, z$p) => {
    if (m$p.tag === "Leaf") { return z$p; }
    if (m$p.tag === "Node") { return go(m$p._5, Data$dList$dTypes.$List("Cons", m$p._3, go(m$p._6, z$p))); }
    $runtime.fail();
  };
  const go$1 = go$1$a0$copy => go$1$a1$copy => {
    let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
    while (go$1$c) {
      const source = go$1$a0, memo = go$1$a1;
      if (source.tag === "Nil") {
        const go$2 = go$2$a0$copy => go$2$a1$copy => {
          let go$2$a0 = go$2$a0$copy, go$2$a1 = go$2$a1$copy, go$2$c = true, go$2$r;
          while (go$2$c) {
            const b = go$2$a0, v = go$2$a1;
            if (v.tag === "Nil") {
              go$2$c = false;
              go$2$r = b;
              continue;
            }
            if (v.tag === "Cons") {
              go$2$a0 = Data$dList$dTypes.$List("Cons", v._1, b);
              go$2$a1 = v._2;
              continue;
            }
            $runtime.fail();
          }
          return go$2$r;
        };
        go$1$c = false;
        go$1$r = go$2(Data$dList$dTypes.Nil)(memo);
        continue;
      }
      if (source.tag === "Cons") {
        go$1$a0 = source._2;
        go$1$a1 = Data$dList$dTypes.$List("Cons", source._1, memo);
        continue;
      }
      $runtime.fail();
    }
    return go$1$r;
  };
  return go$1(go(x, Data$dList$dTypes.Nil))(Data$dList$dTypes.Nil);
};
const fromFoldable = /* #__PURE__ */ Foreign$dObject.fromFoldable(Data$dList$dTypes.foldableList);
const extend = encoder => v => {
  const $0 = Data$dArgonaut$dCore.caseJsonObject(Data$dArgonaut$dCore.jsonSingletonObject(v._1)(v._2))(x => Data$dArgonaut$dCore.fromObject((() => {
    const $0 = v._1;
    const $1 = v._2;
    return Foreign$dObject.mutate($2 => () => {
      $2[$0] = $1;
      return $2;
    })(x);
  })()));
  return x => $0(encoder(x));
};
const extendOptional = encoder => v => {
  if (v.tag === "Just") { return extend(encoder)(v._1); }
  if (v.tag === "Nothing") { return encoder; }
  $runtime.fail();
};
const encodeVoid = Data$dVoid.absurd;
const encodeUnit = v => Data$dArgonaut$dCore.jsonNull;
const encodeTuple = encoderA => encoderB => v => Data$dArgonaut$dCore.fromArray([encoderA(v._1), encoderB(v._2)]);
const encodeString = Data$dArgonaut$dCore.fromString;
const encodeNumber = Data$dArgonaut$dCore.fromNumber;
const encodeNonEmptyString = x => Data$dArgonaut$dCore.fromString(x);
const encodeMaybe = encoder => v => {
  if (v.tag === "Nothing") { return Data$dArgonaut$dCore.jsonNull; }
  if (v.tag === "Just") { return encoder(v._1); }
  $runtime.fail();
};
const encodeList = encoder => {
  const $0 = Data$dFunctor.arrayMap(encoder);
  return x => Data$dArgonaut$dCore.fromArray($0(toUnfoldable(x)));
};
const encodeMap = dictOrd => encoderA => encoderB => {
  const $0 = encodeList(encodeTuple(encoderA)(encoderB));
  return x => $0(toUnfoldable1(x));
};
const encodeNonEmptyList = encoder => {
  const $0 = encodeList(encoder);
  return x => $0(Data$dList$dTypes.$List("Cons", x._1, x._2));
};
const encodeNonEmpty_List = encoder => v => encodeList(encoder)(Data$dList$dTypes.$List("Cons", v._1, v._2));
const encodeSet = dictOrd => encoder => {
  const $0 = encodeList(encoder);
  return x => $0(toUnfoldable2(x));
};
const encodeInt = x => Data$dArgonaut$dCore.fromNumber(Data$dInt.toNumber(x));
const encodeIdentity = encoder => v => encoder(v);
const encodeForeignObject = encoder => x => Data$dArgonaut$dCore.fromObject(Foreign$dObject._fmapObject(x, encoder));
const encodeEither = encoderA => encoderB => v2 => {
  if (v2.tag === "Left") {
    return Data$dArgonaut$dCore.fromObject(fromFoldable(Data$dList$dTypes.$List(
      "Cons",
      Data$dTuple.$Tuple("tag", Data$dArgonaut$dCore.fromString("Left")),
      Data$dList$dTypes.$List("Cons", Data$dTuple.$Tuple("value", encoderA(v2._1)), Data$dList$dTypes.Nil)
    )));
  }
  if (v2.tag === "Right") {
    return Data$dArgonaut$dCore.fromObject(fromFoldable(Data$dList$dTypes.$List(
      "Cons",
      Data$dTuple.$Tuple("tag", Data$dArgonaut$dCore.fromString("Right")),
      Data$dList$dTypes.$List("Cons", Data$dTuple.$Tuple("value", encoderB(v2._1)), Data$dList$dTypes.Nil)
    )));
  }
  $runtime.fail();
};
const encodeCodePoint = x => Data$dArgonaut$dCore.fromString(Data$dString$dCodePoints.singleton(x));
const encodeChar = x => Data$dArgonaut$dCore.fromString(Data$dString$dCodeUnits.singleton(x));
const encodeBoolean = Data$dArgonaut$dCore.fromBoolean;
const encodeArray = encoder => {
  const $0 = Data$dFunctor.arrayMap(encoder);
  return x => Data$dArgonaut$dCore.fromArray($0(x));
};
const encodeNonEmptyArray = encoder => {
  const $0 = Data$dFunctor.arrayMap(encoder);
  return x => Data$dArgonaut$dCore.fromArray($0(x));
};
const encodeNonEmpty_Array = encoder => v => Data$dArgonaut$dCore.fromArray(Data$dFunctor.arrayMap(encoder)([v._1, ...v._2]));
const assocOptional = encoder => k => {
  const $0 = Data$dTuple.Tuple(k);
  return v1 => {
    if (v1.tag === "Just") { return Data$dMaybe.$Maybe("Just", $0(encoder(v1._1))); }
    return Data$dMaybe.Nothing;
  };
};
const assoc = encoder => k => {
  const $0 = Data$dTuple.Tuple(k);
  return x => $0(encoder(x));
};
export {
  assoc,
  assocOptional,
  encodeArray,
  encodeBoolean,
  encodeChar,
  encodeCodePoint,
  encodeEither,
  encodeForeignObject,
  encodeIdentity,
  encodeInt,
  encodeList,
  encodeMap,
  encodeMaybe,
  encodeNonEmptyArray,
  encodeNonEmptyList,
  encodeNonEmptyString,
  encodeNonEmpty_Array,
  encodeNonEmpty_List,
  encodeNumber,
  encodeSet,
  encodeString,
  encodeTuple,
  encodeUnit,
  encodeVoid,
  extend,
  extendOptional,
  fromFoldable,
  toUnfoldable,
  toUnfoldable1,
  toUnfoldable2
};
