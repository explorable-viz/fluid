import * as $runtime from "../runtime.js";
import * as Data$dArgonaut$dCore from "../Data.Argonaut.Core/index.js";
import * as Data$dArgonaut$dDecode$dDecoders from "../Data.Argonaut.Decode.Decoders/index.js";
import * as Data$dArgonaut$dDecode$dError from "../Data.Argonaut.Decode.Error/index.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Foreign$dObject from "../Foreign.Object/index.js";
import * as Record$dUnsafe from "../Record.Unsafe/index.js";
import * as Type$dProxy from "../Type.Proxy/index.js";
const gDecodeJsonNil = {gDecodeJson: v => v1 => Data$dEither.$Either("Right", {})};
const gDecodeJson = dict => dict.gDecodeJson;
const decodeVoid = {decodeJson: Data$dArgonaut$dDecode$dDecoders.decodeVoid};
const decodeRecord = dictGDecodeJson => () => (
  {
    decodeJson: json => {
      const v = Data$dArgonaut$dCore._caseJson(
        v => Data$dMaybe.Nothing,
        v => Data$dMaybe.Nothing,
        v => Data$dMaybe.Nothing,
        v => Data$dMaybe.Nothing,
        v => Data$dMaybe.Nothing,
        Data$dMaybe.Just,
        json
      );
      if (v.tag === "Just") { return dictGDecodeJson.gDecodeJson(v._1)(Type$dProxy.Proxy); }
      if (v.tag === "Nothing") { return Data$dEither.$Either("Left", Data$dArgonaut$dDecode$dError.$JsonDecodeError("TypeMismatch", "Object")); }
      $runtime.fail();
    }
  }
);
const decodeJsonString = {
  decodeJson: /* #__PURE__ */ Data$dArgonaut$dCore.caseJsonString(/* #__PURE__ */ Data$dEither.$Either(
    "Left",
    /* #__PURE__ */ Data$dArgonaut$dDecode$dError.$JsonDecodeError("TypeMismatch", "String")
  ))(Data$dEither.Right)
};
const decodeJsonNumber = {
  decodeJson: /* #__PURE__ */ Data$dArgonaut$dCore.caseJsonNumber(/* #__PURE__ */ Data$dEither.$Either(
    "Left",
    /* #__PURE__ */ Data$dArgonaut$dDecode$dError.$JsonDecodeError("TypeMismatch", "Number")
  ))(Data$dEither.Right)
};
const decodeJsonNull = {decodeJson: Data$dArgonaut$dDecode$dDecoders.decodeNull};
const decodeJsonNonEmptyString = {decodeJson: Data$dArgonaut$dDecode$dDecoders.decodeNonEmptyString};
const decodeJsonJson = {decodeJson: Data$dEither.Right};
const decodeJsonInt = {decodeJson: Data$dArgonaut$dDecode$dDecoders.decodeInt};
const decodeJsonField = dict => dict.decodeJsonField;
const gDecodeJsonCons = dictDecodeJsonField => dictGDecodeJson => dictIsSymbol => () => () => (
  {
    gDecodeJson: object => v => {
      const fieldName = dictIsSymbol.reflectSymbol(Type$dProxy.Proxy);
      const v1 = dictDecodeJsonField.decodeJsonField(Foreign$dObject._lookup(Data$dMaybe.Nothing, Data$dMaybe.Just, fieldName, object));
      if (v1.tag === "Just") {
        const $0 = Data$dArgonaut$dDecode$dError.AtKey(fieldName);
        if (v1._1.tag === "Left") { return Data$dEither.$Either("Left", $0(v1._1._1)); }
        if (v1._1.tag === "Right") {
          const $1 = v1._1._1;
          const $2 = dictGDecodeJson.gDecodeJson(object)(Type$dProxy.Proxy);
          return (() => {
            if ($2.tag === "Left") {
              const $3 = $2._1;
              return v$1 => Data$dEither.$Either("Left", $3);
            }
            if ($2.tag === "Right") {
              const $3 = $2._1;
              return f => f($3);
            }
            $runtime.fail();
          })()(rest => Data$dEither.$Either("Right", Record$dUnsafe.unsafeSet(dictIsSymbol.reflectSymbol(Type$dProxy.Proxy))($1)(rest)));
        }
        $runtime.fail();
      }
      if (v1.tag === "Nothing") {
        return Data$dEither.$Either("Left", Data$dArgonaut$dDecode$dError.$JsonDecodeError("AtKey", fieldName, Data$dArgonaut$dDecode$dError.MissingValue));
      }
      $runtime.fail();
    }
  }
);
const decodeJsonCodePoint = {decodeJson: Data$dArgonaut$dDecode$dDecoders.decodeCodePoint};
const decodeJsonBoolean = {
  decodeJson: /* #__PURE__ */ Data$dArgonaut$dCore.caseJsonBoolean(/* #__PURE__ */ Data$dEither.$Either(
    "Left",
    /* #__PURE__ */ Data$dArgonaut$dDecode$dError.$JsonDecodeError("TypeMismatch", "Boolean")
  ))(Data$dEither.Right)
};
const decodeJson = dict => dict.decodeJson;
const decodeJsonEither = dictDecodeJson => {
  const decodeJson1 = dictDecodeJson.decodeJson;
  return dictDecodeJson1 => ({decodeJson: Data$dArgonaut$dDecode$dDecoders.decodeEither(decodeJson1)(dictDecodeJson1.decodeJson)});
};
const decodeJsonMaybe = dictDecodeJson => ({decodeJson: Data$dArgonaut$dDecode$dDecoders.decodeMaybe(dictDecodeJson.decodeJson)});
const decodeJsonNonEmptyArray = dictDecodeJson => ({decodeJson: Data$dArgonaut$dDecode$dDecoders.decodeNonEmptyArray(dictDecodeJson.decodeJson)});
const decodeJsonNonEmptyList = dictDecodeJson => ({decodeJson: Data$dArgonaut$dDecode$dDecoders.decodeNonEmptyList(dictDecodeJson.decodeJson)});
const decodeJsonNonEmpty_Array = dictDecodeJson => ({decodeJson: Data$dArgonaut$dDecode$dDecoders.decodeNonEmpty_Array(dictDecodeJson.decodeJson)});
const decodeJsonNonEmpty_List = dictDecodeJson => ({decodeJson: Data$dArgonaut$dDecode$dDecoders.decodeNonEmpty_List(dictDecodeJson.decodeJson)});
const decodeJsonTuple = dictDecodeJson => {
  const decodeJson1 = dictDecodeJson.decodeJson;
  return dictDecodeJson1 => ({decodeJson: Data$dArgonaut$dDecode$dDecoders.decodeTuple(decodeJson1)(dictDecodeJson1.decodeJson)});
};
const decodeList = dictDecodeJson => ({decodeJson: Data$dArgonaut$dDecode$dDecoders.decodeList(dictDecodeJson.decodeJson)});
const decodeMap = dictOrd => {
  const decodeMap1 = Data$dArgonaut$dDecode$dDecoders.decodeMap(dictOrd);
  return dictDecodeJson => {
    const decodeJson1 = dictDecodeJson.decodeJson;
    return dictDecodeJson1 => ({decodeJson: decodeMap1(decodeJson1)(dictDecodeJson1.decodeJson)});
  };
};
const decodeSet = dictOrd => {
  const decodeSet1 = Data$dArgonaut$dDecode$dDecoders.decodeSet(dictOrd);
  return dictDecodeJson => ({decodeJson: decodeSet1(dictDecodeJson.decodeJson)});
};
const decodeIdentity = dictDecodeJson => (
  {
    decodeJson: json => {
      const $0 = dictDecodeJson.decodeJson(json);
      if ($0.tag === "Left") { return Data$dEither.$Either("Left", $0._1); }
      if ($0.tag === "Right") { return Data$dEither.$Either("Right", $0._1); }
      $runtime.fail();
    }
  }
);
const decodeForeignObject = dictDecodeJson => ({decodeJson: Data$dArgonaut$dDecode$dDecoders.decodeForeignObject(dictDecodeJson.decodeJson)});
const decodeFieldMaybe = dictDecodeJson => (
  {
    decodeJsonField: v => {
      if (v.tag === "Nothing") { return Data$dMaybe.$Maybe("Just", Data$dEither.$Either("Right", Data$dMaybe.Nothing)); }
      if (v.tag === "Just") { return Data$dMaybe.$Maybe("Just", Data$dArgonaut$dDecode$dDecoders.decodeMaybe(dictDecodeJson.decodeJson)(v._1)); }
      $runtime.fail();
    }
  }
);
const decodeFieldId = dictDecodeJson => (
  {
    decodeJsonField: j => {
      if (j.tag === "Just") { return Data$dMaybe.$Maybe("Just", dictDecodeJson.decodeJson(j._1)); }
      return Data$dMaybe.Nothing;
    }
  }
);
const decodeArray = dictDecodeJson => ({decodeJson: Data$dArgonaut$dDecode$dDecoders.decodeArray(dictDecodeJson.decodeJson)});
export {
  
  
  decodeFieldMaybe,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  decodeRecord,
  
  
  
  gDecodeJsonCons,
  gDecodeJsonNil
};
