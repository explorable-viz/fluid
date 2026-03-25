import * as $runtime from "../runtime.js";
import * as Data$dArgonaut$dCore from "../Data.Argonaut.Core/index.js";
import * as Data$dArgonaut$dDecode$dError from "../Data.Argonaut.Decode.Error/index.js";
import * as Data$dArray from "../Data.Array/index.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dInt from "../Data.Int/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dMap$dInternal from "../Data.Map.Internal/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNonEmpty from "../Data.NonEmpty/index.js";
import * as Data$dString$dCodePoints from "../Data.String.CodePoints/index.js";
import * as Data$dTraversable from "../Data.Traversable/index.js";
import * as Data$dTraversableWithIndex from "../Data.TraversableWithIndex/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Foreign$dObject from "../Foreign.Object/index.js";
const traverse = /* #__PURE__ */ (() => Data$dList$dTypes.traversableList.traverse(Data$dEither.applicativeEither))();
const fromFoldable = /* #__PURE__ */ Data$dFoldable.foldrArray(Data$dList$dTypes.Cons)(Data$dList$dTypes.Nil);
const traverse1 = /* #__PURE__ */ (() => Data$dTraversable.traversableArray.traverse(Data$dEither.applicativeEither))();
const traverse2 = /* #__PURE__ */ (() => Data$dList$dTypes.traversableNonEmptyList.traverse(Data$dEither.applicativeEither))();
const traverse3 = /* #__PURE__ */ (() => Data$dNonEmpty.traversableNonEmpty(Data$dTraversable.traversableArray).traverse(Data$dEither.applicativeEither))();
const traverse4 = /* #__PURE__ */ (() => Data$dNonEmpty.traversableNonEmpty(Data$dList$dTypes.traversableList).traverse(Data$dEither.applicativeEither))();
const traverse5 = /* #__PURE__ */ (() => {
  const $0 = Foreign$dObject.traversableWithIndexObject.traverseWithIndex(Data$dEither.applicativeEither);
  return x => $0(v => x);
})();
const traverseWithIndex = /* #__PURE__ */ (() => Data$dTraversableWithIndex.traversableWithIndexArray.traverseWithIndex(Data$dEither.applicativeEither))();
const getFieldOptional$p = decoder => obj => str => {
  const $0 = Foreign$dObject._lookup(Data$dMaybe.Nothing, Data$dMaybe.Just, str, obj);
  if ($0.tag === "Nothing") { return Data$dEither.$Either("Right", Data$dMaybe.Nothing); }
  if ($0.tag === "Just") {
    if (Data$dArgonaut$dCore._caseJson(v => true, v => false, v => false, v => false, v => false, v => false, $0._1)) { return Data$dEither.$Either("Right", Data$dMaybe.Nothing); }
    const $1 = Data$dArgonaut$dDecode$dError.AtKey(str);
    const $2 = decoder($0._1);
    if ($2.tag === "Left") { return Data$dEither.$Either("Left", $1($2._1)); }
    if ($2.tag === "Right") { return Data$dEither.$Either("Right", Data$dMaybe.$Maybe("Just", $2._1)); }
  }
  $runtime.fail();
};
const getFieldOptional = decoder => obj => str => {
  const $0 = Data$dArgonaut$dDecode$dError.AtKey(str);
  const $1 = Foreign$dObject._lookup(Data$dMaybe.Nothing, Data$dMaybe.Just, str, obj);
  if ($1.tag === "Nothing") { return Data$dEither.$Either("Right", Data$dMaybe.Nothing); }
  if ($1.tag === "Just") {
    const $2 = decoder($1._1);
    if ($2.tag === "Left") { return Data$dEither.$Either("Left", $0($2._1)); }
    if ($2.tag === "Right") { return Data$dEither.$Either("Right", Data$dMaybe.$Maybe("Just", $2._1)); }
  }
  $runtime.fail();
};
const getField = decoder => obj => str => {
  const $0 = Data$dArgonaut$dDecode$dError.AtKey(str);
  const $1 = Foreign$dObject._lookup(Data$dMaybe.Nothing, Data$dMaybe.Just, str, obj);
  if ($1.tag === "Nothing") { return Data$dEither.$Either("Left", Data$dArgonaut$dDecode$dError.$JsonDecodeError("AtKey", str, Data$dArgonaut$dDecode$dError.MissingValue)); }
  if ($1.tag === "Just") {
    const $2 = decoder($1._1);
    if ($2.tag === "Left") { return Data$dEither.$Either("Left", $0($2._1)); }
    if ($2.tag === "Right") { return Data$dEither.$Either("Right", $2._1); }
  }
  $runtime.fail();
};
const decodeVoid = v => Data$dEither.$Either("Left", Data$dArgonaut$dDecode$dError.$JsonDecodeError("UnexpectedValue", Data$dArgonaut$dCore.fromString("Value cannot be Void")));
const decodeString = /* #__PURE__ */ Data$dArgonaut$dCore.caseJsonString(/* #__PURE__ */ Data$dEither.$Either(
  "Left",
  /* #__PURE__ */ Data$dArgonaut$dDecode$dError.$JsonDecodeError("TypeMismatch", "String")
))(Data$dEither.Right);
const decodeNumber = /* #__PURE__ */ Data$dArgonaut$dCore.caseJsonNumber(/* #__PURE__ */ Data$dEither.$Either(
  "Left",
  /* #__PURE__ */ Data$dArgonaut$dDecode$dError.$JsonDecodeError("TypeMismatch", "Number")
))(Data$dEither.Right);
const decodeNull = /* #__PURE__ */ Data$dArgonaut$dCore.caseJsonNull(/* #__PURE__ */ Data$dEither.$Either(
  "Left",
  /* #__PURE__ */ Data$dArgonaut$dDecode$dError.$JsonDecodeError("TypeMismatch", "null")
))(v => Data$dEither.$Either("Right", undefined));
const decodeNonEmptyString = json => {
  const $0 = Data$dArgonaut$dCore._caseJson(
    v => Data$dEither.$Either("Left", Data$dArgonaut$dDecode$dError.$JsonDecodeError("TypeMismatch", "String")),
    v => Data$dEither.$Either("Left", Data$dArgonaut$dDecode$dError.$JsonDecodeError("TypeMismatch", "String")),
    v => Data$dEither.$Either("Left", Data$dArgonaut$dDecode$dError.$JsonDecodeError("TypeMismatch", "String")),
    Data$dEither.Right,
    v => Data$dEither.$Either("Left", Data$dArgonaut$dDecode$dError.$JsonDecodeError("TypeMismatch", "String")),
    v => Data$dEither.$Either("Left", Data$dArgonaut$dDecode$dError.$JsonDecodeError("TypeMismatch", "String")),
    json
  );
  if ($0.tag === "Left") { return Data$dEither.$Either("Left", $0._1); }
  if ($0.tag === "Right") {
    if ($0._1 === "") {
      return Data$dEither.$Either(
        "Left",
        Data$dArgonaut$dDecode$dError.$JsonDecodeError("Named", "NonEmptyString", Data$dArgonaut$dDecode$dError.$JsonDecodeError("UnexpectedValue", json))
      );
    }
    return Data$dEither.$Either("Right", $0._1);
  }
  $runtime.fail();
};
const decodeMaybe = decoder => json => {
  if (Data$dArgonaut$dCore._caseJson(v => true, v => false, v => false, v => false, v => false, v => false, json)) { return Data$dEither.$Either("Right", Data$dMaybe.Nothing); }
  const $0 = decoder(json);
  if ($0.tag === "Left") { return Data$dEither.$Either("Left", $0._1); }
  if ($0.tag === "Right") { return Data$dEither.$Either("Right", Data$dMaybe.$Maybe("Just", $0._1)); }
  $runtime.fail();
};
const decodeJObject = x => {
  const $0 = Data$dArgonaut$dCore._caseJson(
    v => Data$dMaybe.Nothing,
    v => Data$dMaybe.Nothing,
    v => Data$dMaybe.Nothing,
    v => Data$dMaybe.Nothing,
    v => Data$dMaybe.Nothing,
    Data$dMaybe.Just,
    x
  );
  if ($0.tag === "Nothing") { return Data$dEither.$Either("Left", Data$dArgonaut$dDecode$dError.$JsonDecodeError("TypeMismatch", "Object")); }
  if ($0.tag === "Just") { return Data$dEither.$Either("Right", $0._1); }
  $runtime.fail();
};
const decodeJArray = x => {
  const $0 = Data$dArgonaut$dCore._caseJson(
    v => Data$dMaybe.Nothing,
    v => Data$dMaybe.Nothing,
    v => Data$dMaybe.Nothing,
    v => Data$dMaybe.Nothing,
    Data$dMaybe.Just,
    v => Data$dMaybe.Nothing,
    x
  );
  if ($0.tag === "Nothing") { return Data$dEither.$Either("Left", Data$dArgonaut$dDecode$dError.$JsonDecodeError("TypeMismatch", "Array")); }
  if ($0.tag === "Just") { return Data$dEither.$Either("Right", $0._1); }
  $runtime.fail();
};
const decodeList = decoder => {
  const $0 = Data$dArgonaut$dDecode$dError.Named("List");
  const $1 = traverse(decoder);
  return a => {
    const $2 = decodeJArray(a);
    if ($2.tag === "Left") { return Data$dEither.$Either("Left", $2._1); }
    if ($2.tag === "Right") {
      const $3 = $1(fromFoldable($2._1));
      if ($3.tag === "Left") { return Data$dEither.$Either("Left", $0($3._1)); }
      if ($3.tag === "Right") { return Data$dEither.$Either("Right", $3._1); }
    }
    $runtime.fail();
  };
};
const decodeSet = dictOrd => {
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
        go$a0 = Data$dMap$dInternal.insert(dictOrd)(v._1)()(b);
        go$a1 = v._2;
        continue;
      }
      $runtime.fail();
    }
    return go$r;
  };
  const fromFoldable2 = go(Data$dMap$dInternal.Leaf);
  return decoder => {
    const $0 = decodeList(decoder);
    return x => {
      const $1 = $0(x);
      if ($1.tag === "Left") { return Data$dEither.$Either("Left", $1._1); }
      if ($1.tag === "Right") { return Data$dEither.$Either("Right", fromFoldable2($1._1)); }
      $runtime.fail();
    };
  };
};
const decodeNonEmptyArray = decoder => {
  const $0 = Data$dArgonaut$dDecode$dError.Named("NonEmptyArray");
  const $1 = traverse1(decoder);
  return a => {
    const $2 = decodeJArray(a);
    if ($2.tag === "Left") { return Data$dEither.$Either("Left", $2._1); }
    if ($2.tag === "Right") {
      const $3 = Data$dArray.unconsImpl(v => Data$dMaybe.Nothing, x => xs => Data$dMaybe.$Maybe("Just", {head: x, tail: xs}), $2._1);
      if ($3.tag === "Nothing") { return Data$dEither.$Either("Left", Data$dArgonaut$dDecode$dError.$JsonDecodeError("TypeMismatch", "NonEmptyArray")); }
      if ($3.tag === "Just") {
        const $4 = $1([$3._1.head, ...$3._1.tail]);
        if ($4.tag === "Left") { return Data$dEither.$Either("Left", $0($4._1)); }
        if ($4.tag === "Right") { return Data$dEither.$Either("Right", $4._1); }
      }
    }
    $runtime.fail();
  };
};
const decodeNonEmptyList = decoder => {
  const $0 = Data$dArgonaut$dDecode$dError.Named("NonEmptyList");
  const $1 = traverse2(decoder);
  return a => {
    const $2 = decodeJArray(a);
    if ($2.tag === "Left") { return Data$dEither.$Either("Left", $2._1); }
    if ($2.tag === "Right") {
      if (fromFoldable($2._1).tag === "Nil") { return Data$dEither.$Either("Left", Data$dArgonaut$dDecode$dError.$JsonDecodeError("TypeMismatch", "NonEmptyList")); }
      if (fromFoldable($2._1).tag === "Cons") {
        const $3 = $1(Data$dNonEmpty.$NonEmpty(fromFoldable($2._1)._1, fromFoldable($2._1)._2));
        if ($3.tag === "Left") { return Data$dEither.$Either("Left", $0($3._1)); }
        if ($3.tag === "Right") { return Data$dEither.$Either("Right", $3._1); }
      }
    }
    $runtime.fail();
  };
};
const decodeNonEmpty_Array = decoder => {
  const $0 = Data$dArgonaut$dDecode$dError.Named("NonEmpty Array");
  const $1 = traverse3(decoder);
  return a => {
    const $2 = decodeJArray(a);
    if ($2.tag === "Left") { return Data$dEither.$Either("Left", $2._1); }
    if ($2.tag === "Right") {
      const $3 = Data$dArray.unconsImpl(v => Data$dMaybe.Nothing, x => xs => Data$dMaybe.$Maybe("Just", {head: x, tail: xs}), $2._1);
      if ($3.tag === "Nothing") { return Data$dEither.$Either("Left", Data$dArgonaut$dDecode$dError.$JsonDecodeError("TypeMismatch", "NonEmpty Array")); }
      if ($3.tag === "Just") {
        const $4 = $1(Data$dNonEmpty.$NonEmpty($3._1.head, $3._1.tail));
        if ($4.tag === "Left") { return Data$dEither.$Either("Left", $0($4._1)); }
        if ($4.tag === "Right") { return Data$dEither.$Either("Right", $4._1); }
      }
    }
    $runtime.fail();
  };
};
const decodeNonEmpty_List = decoder => {
  const $0 = Data$dArgonaut$dDecode$dError.Named("NonEmpty List");
  const $1 = traverse4(decoder);
  return a => {
    const $2 = decodeJArray(a);
    if ($2.tag === "Left") { return Data$dEither.$Either("Left", $2._1); }
    if ($2.tag === "Right") {
      if (fromFoldable($2._1).tag === "Nil") { return Data$dEither.$Either("Left", Data$dArgonaut$dDecode$dError.$JsonDecodeError("TypeMismatch", "NonEmpty List")); }
      if (fromFoldable($2._1).tag === "Cons") {
        const $3 = $1(Data$dNonEmpty.$NonEmpty(fromFoldable($2._1)._1, fromFoldable($2._1)._2));
        if ($3.tag === "Left") { return Data$dEither.$Either("Left", $0($3._1)); }
        if ($3.tag === "Right") { return Data$dEither.$Either("Right", $3._1); }
      }
    }
    $runtime.fail();
  };
};
const decodeInt = a => {
  const $0 = Data$dArgonaut$dCore._caseJson(
    v => Data$dEither.$Either("Left", Data$dArgonaut$dDecode$dError.$JsonDecodeError("TypeMismatch", "Number")),
    v => Data$dEither.$Either("Left", Data$dArgonaut$dDecode$dError.$JsonDecodeError("TypeMismatch", "Number")),
    Data$dEither.Right,
    v => Data$dEither.$Either("Left", Data$dArgonaut$dDecode$dError.$JsonDecodeError("TypeMismatch", "Number")),
    v => Data$dEither.$Either("Left", Data$dArgonaut$dDecode$dError.$JsonDecodeError("TypeMismatch", "Number")),
    v => Data$dEither.$Either("Left", Data$dArgonaut$dDecode$dError.$JsonDecodeError("TypeMismatch", "Number")),
    a
  );
  if ($0.tag === "Left") { return Data$dEither.$Either("Left", $0._1); }
  if ($0.tag === "Right") {
    const $1 = Data$dInt.fromNumber($0._1);
    if ($1.tag === "Nothing") { return Data$dEither.$Either("Left", Data$dArgonaut$dDecode$dError.$JsonDecodeError("TypeMismatch", "Integer")); }
    if ($1.tag === "Just") { return Data$dEither.$Either("Right", $1._1); }
  }
  $runtime.fail();
};
const decodeIdentity = decoder => json => {
  const $0 = decoder(json);
  if ($0.tag === "Left") { return Data$dEither.$Either("Left", $0._1); }
  if ($0.tag === "Right") { return Data$dEither.$Either("Right", $0._1); }
  $runtime.fail();
};
const decodeForeignObject = decoder => {
  const $0 = Data$dArgonaut$dDecode$dError.Named("ForeignObject");
  const $1 = traverse5(decoder);
  return a => {
    const $2 = decodeJObject(a);
    if ($2.tag === "Left") { return Data$dEither.$Either("Left", $2._1); }
    if ($2.tag === "Right") {
      const $3 = $1($2._1);
      if ($3.tag === "Left") { return Data$dEither.$Either("Left", $0($3._1)); }
      if ($3.tag === "Right") { return Data$dEither.$Either("Right", $3._1); }
    }
    $runtime.fail();
  };
};
const decodeEither = decoderA => decoderB => json => {
  const $0 = Data$dArgonaut$dDecode$dError.Named("Either");
  const $1 = decodeJObject(json);
  const $2 = (() => {
    if ($1.tag === "Left") {
      const $2 = $1._1;
      return v => Data$dEither.$Either("Left", $2);
    }
    if ($1.tag === "Right") {
      const $2 = $1._1;
      return f => f($2);
    }
    $runtime.fail();
  })()(obj => {
    const $2 = Foreign$dObject._lookup(Data$dMaybe.Nothing, Data$dMaybe.Just, "tag", obj);
    if ($2.tag === "Nothing") { return Data$dEither.$Either("Left", Data$dArgonaut$dDecode$dError.$JsonDecodeError("AtKey", "tag", Data$dArgonaut$dDecode$dError.MissingValue)); }
    if ($2.tag === "Just") {
      const $3 = Foreign$dObject._lookup(Data$dMaybe.Nothing, Data$dMaybe.Just, "value", obj);
      if ($3.tag === "Nothing") {
        return Data$dEither.$Either("Left", Data$dArgonaut$dDecode$dError.$JsonDecodeError("AtKey", "value", Data$dArgonaut$dDecode$dError.MissingValue));
      }
      if ($3.tag === "Just") {
        const v = Data$dArgonaut$dCore._caseJson(
          v => Data$dMaybe.Nothing,
          v => Data$dMaybe.Nothing,
          v => Data$dMaybe.Nothing,
          Data$dMaybe.Just,
          v => Data$dMaybe.Nothing,
          v => Data$dMaybe.Nothing,
          $2._1
        );
        if (v.tag === "Just") {
          if (v._1 === "Right") {
            const $4 = decoderB($3._1);
            if ($4.tag === "Left") { return Data$dEither.$Either("Left", $4._1); }
            if ($4.tag === "Right") { return Data$dEither.$Either("Right", Data$dEither.$Either("Right", $4._1)); }
            $runtime.fail();
          }
          if (v._1 === "Left") {
            const $4 = decoderA($3._1);
            if ($4.tag === "Left") { return Data$dEither.$Either("Left", $4._1); }
            if ($4.tag === "Right") { return Data$dEither.$Either("Right", Data$dEither.$Either("Left", $4._1)); }
            $runtime.fail();
          }
        }
        return Data$dEither.$Either(
          "Left",
          Data$dArgonaut$dDecode$dError.$JsonDecodeError("AtKey", "tag", Data$dArgonaut$dDecode$dError.$JsonDecodeError("UnexpectedValue", $2._1))
        );
      }
    }
    $runtime.fail();
  });
  if ($2.tag === "Left") { return Data$dEither.$Either("Left", $0($2._1)); }
  if ($2.tag === "Right") { return Data$dEither.$Either("Right", $2._1); }
  $runtime.fail();
};
const decodeCodePoint = json => {
  const $0 = Data$dArgonaut$dCore._caseJson(
    v => Data$dEither.$Either("Left", Data$dArgonaut$dDecode$dError.$JsonDecodeError("TypeMismatch", "String")),
    v => Data$dEither.$Either("Left", Data$dArgonaut$dDecode$dError.$JsonDecodeError("TypeMismatch", "String")),
    v => Data$dEither.$Either("Left", Data$dArgonaut$dDecode$dError.$JsonDecodeError("TypeMismatch", "String")),
    Data$dEither.Right,
    v => Data$dEither.$Either("Left", Data$dArgonaut$dDecode$dError.$JsonDecodeError("TypeMismatch", "String")),
    v => Data$dEither.$Either("Left", Data$dArgonaut$dDecode$dError.$JsonDecodeError("TypeMismatch", "String")),
    json
  );
  if ($0.tag === "Left") { return Data$dEither.$Either("Left", $0._1); }
  if ($0.tag === "Right") {
    const $1 = Data$dString$dCodePoints.codePointAt(0)($0._1);
    if ($1.tag === "Nothing") {
      return Data$dEither.$Either(
        "Left",
        Data$dArgonaut$dDecode$dError.$JsonDecodeError("Named", "CodePoint", Data$dArgonaut$dDecode$dError.$JsonDecodeError("UnexpectedValue", json))
      );
    }
    if ($1.tag === "Just") { return Data$dEither.$Either("Right", $1._1); }
  }
  $runtime.fail();
};
const decodeBoolean = /* #__PURE__ */ Data$dArgonaut$dCore.caseJsonBoolean(/* #__PURE__ */ Data$dEither.$Either(
  "Left",
  /* #__PURE__ */ Data$dArgonaut$dDecode$dError.$JsonDecodeError("TypeMismatch", "Boolean")
))(Data$dEither.Right);
const decodeArray = decoder => {
  const $0 = Data$dArgonaut$dDecode$dError.Named("Array");
  const $1 = traverseWithIndex(i => {
    const $1 = Data$dArgonaut$dDecode$dError.AtIndex(i);
    return x => {
      const $2 = decoder(x);
      if ($2.tag === "Left") { return Data$dEither.$Either("Left", $1($2._1)); }
      if ($2.tag === "Right") { return Data$dEither.$Either("Right", $2._1); }
      $runtime.fail();
    };
  });
  return a => {
    const $2 = decodeJArray(a);
    if ($2.tag === "Left") { return Data$dEither.$Either("Left", $2._1); }
    if ($2.tag === "Right") {
      const $3 = $1($2._1);
      if ($3.tag === "Left") { return Data$dEither.$Either("Left", $0($3._1)); }
      if ($3.tag === "Right") { return Data$dEither.$Either("Right", $3._1); }
    }
    $runtime.fail();
  };
};
const decodeTuple = decoderA => decoderB => json => {
  const $0 = decodeArray(Data$dEither.Right)(json);
  return (() => {
    if ($0.tag === "Left") {
      const $1 = $0._1;
      return v => Data$dEither.$Either("Left", $1);
    }
    if ($0.tag === "Right") {
      const $1 = $0._1;
      return f => f($1);
    }
    $runtime.fail();
  })()(v => {
    if (v.length === 2) {
      const $1 = decoderA(v[0]);
      if ($1.tag === "Left") { return Data$dEither.$Either("Left", $1._1); }
      if ($1.tag === "Right") {
        const $2 = decoderB(v[1]);
        if ($2.tag === "Left") { return Data$dEither.$Either("Left", $2._1); }
        if ($2.tag === "Right") { return Data$dEither.$Either("Right", Data$dTuple.$Tuple($1._1, $2._1)); }
      }
      $runtime.fail();
    }
    return Data$dEither.$Either("Left", Data$dArgonaut$dDecode$dError.$JsonDecodeError("TypeMismatch", "Tuple"));
  });
};
const decodeMap = dictOrd => {
  const fromFoldable2 = Data$dMap$dInternal.fromFoldable(dictOrd)(Data$dList$dTypes.foldableList);
  return decoderA => decoderB => {
    const $0 = decodeList(decodeTuple(decoderA)(decoderB));
    return x => {
      const $1 = $0(x);
      if ($1.tag === "Left") { return Data$dEither.$Either("Left", $1._1); }
      if ($1.tag === "Right") { return Data$dEither.$Either("Right", fromFoldable2($1._1)); }
      $runtime.fail();
    };
  };
};
export {
  decodeArray,
  
  decodeCodePoint,
  decodeEither,
  decodeForeignObject,
  
  decodeInt,
  
  
  decodeList,
  decodeMap,
  decodeMaybe,
  decodeNonEmptyArray,
  decodeNonEmptyList,
  decodeNonEmptyString,
  decodeNonEmpty_Array,
  decodeNonEmpty_List,
  decodeNull,
  
  decodeSet,
  
  decodeTuple,
  decodeVoid,
  
  
  
  
  
  
  
  
  
  
  
};
