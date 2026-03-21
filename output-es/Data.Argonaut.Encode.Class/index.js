import * as Data$dArgonaut$dCore from "../Data.Argonaut.Core/index.js";
import * as Data$dArgonaut$dEncode$dEncoders from "../Data.Argonaut.Encode.Encoders/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dVoid from "../Data.Void/index.js";
import * as Foreign$dObject from "../Foreign.Object/index.js";
import * as Record$dUnsafe from "../Record.Unsafe/index.js";
import * as Type$dProxy from "../Type.Proxy/index.js";
const gEncodeJsonNil = {gEncodeJson: v => v1 => Foreign$dObject.empty};
const gEncodeJson = dict => dict.gEncodeJson;
const encodeVoid = {encodeJson: Data$dVoid.absurd};
const encodeRecord = dictGEncodeJson => () => ({encodeJson: rec => Data$dArgonaut$dCore.fromObject(dictGEncodeJson.gEncodeJson(rec)(Type$dProxy.Proxy))});
const encodeNonEmptyString = {encodeJson: Data$dArgonaut$dEncode$dEncoders.encodeNonEmptyString};
const encodeJsonUnit = {encodeJson: Data$dArgonaut$dEncode$dEncoders.encodeUnit};
const encodeJsonJson = {encodeJson: x => x};
const encodeJsonJString = {encodeJson: Data$dArgonaut$dCore.fromString};
const encodeJsonJNumber = {encodeJson: Data$dArgonaut$dCore.fromNumber};
const encodeJsonJBoolean = {encodeJson: Data$dArgonaut$dCore.fromBoolean};
const encodeJsonInt = {encodeJson: Data$dArgonaut$dEncode$dEncoders.encodeInt};
const encodeJsonCodePoint = {encodeJson: Data$dArgonaut$dEncode$dEncoders.encodeCodePoint};
const encodeJsonChar = {encodeJson: Data$dArgonaut$dEncode$dEncoders.encodeChar};
const encodeJson = dict => dict.encodeJson;
const encodeJsonArray = dictEncodeJson => (
  {
    encodeJson: (() => {
      const $0 = Data$dFunctor.arrayMap(dictEncodeJson.encodeJson);
      return x => Data$dArgonaut$dCore.fromArray($0(x));
    })()
  }
);
const encodeJsonEither = dictEncodeJson => {
  const encodeJson1 = dictEncodeJson.encodeJson;
  return dictEncodeJson1 => ({encodeJson: Data$dArgonaut$dEncode$dEncoders.encodeEither(encodeJson1)(dictEncodeJson1.encodeJson)});
};
const encodeJsonList = dictEncodeJson => ({encodeJson: Data$dArgonaut$dEncode$dEncoders.encodeList(dictEncodeJson.encodeJson)});
const encodeJsonMaybe = dictEncodeJson => ({encodeJson: Data$dArgonaut$dEncode$dEncoders.encodeMaybe(dictEncodeJson.encodeJson)});
const encodeJsonNonEmptyArray = dictEncodeJson => (
  {
    encodeJson: (() => {
      const $0 = Data$dFunctor.arrayMap(dictEncodeJson.encodeJson);
      return x => Data$dArgonaut$dCore.fromArray($0(x));
    })()
  }
);
const encodeJsonNonEmptyList = dictEncodeJson => (
  {
    encodeJson: (() => {
      const $0 = Data$dArgonaut$dEncode$dEncoders.encodeList(dictEncodeJson.encodeJson);
      return x => $0(Data$dList$dTypes.$List("Cons", x._1, x._2));
    })()
  }
);
const encodeJsonNonEmpty_Array = dictEncodeJson => ({encodeJson: Data$dArgonaut$dEncode$dEncoders.encodeNonEmpty_Array(dictEncodeJson.encodeJson)});
const encodeJsonNonEmpty_List = dictEncodeJson => ({encodeJson: Data$dArgonaut$dEncode$dEncoders.encodeNonEmpty_List(dictEncodeJson.encodeJson)});
const encodeJsonTuple = dictEncodeJson => {
  const encodeJson1 = dictEncodeJson.encodeJson;
  return dictEncodeJson1 => ({encodeJson: Data$dArgonaut$dEncode$dEncoders.encodeTuple(encodeJson1)(dictEncodeJson1.encodeJson)});
};
const encodeMap = dictOrd => dictEncodeJson => {
  const encodeJson1 = dictEncodeJson.encodeJson;
  return dictEncodeJson1 => ({encodeJson: Data$dArgonaut$dEncode$dEncoders.encodeMap(dictOrd)(encodeJson1)(dictEncodeJson1.encodeJson)});
};
const encodeSet = dictOrd => dictEncodeJson => (
  {
    encodeJson: (() => {
      const $0 = Data$dArgonaut$dEncode$dEncoders.encodeList(dictEncodeJson.encodeJson);
      return x => $0(Data$dArgonaut$dEncode$dEncoders.toUnfoldable2(x));
    })()
  }
);
const gEncodeJsonCons = dictEncodeJson => dictGEncodeJson => dictIsSymbol => () => (
  {
    gEncodeJson: row => v => {
      const $0 = dictIsSymbol.reflectSymbol(Type$dProxy.Proxy);
      const $1 = dictEncodeJson.encodeJson(Record$dUnsafe.unsafeGet(dictIsSymbol.reflectSymbol(Type$dProxy.Proxy))(row));
      return Foreign$dObject.mutate($2 => () => {
        $2[$0] = $1;
        return $2;
      })(dictGEncodeJson.gEncodeJson(row)(Type$dProxy.Proxy));
    }
  }
);
const encodeIdentity = dictEncodeJson => ({encodeJson: v => dictEncodeJson.encodeJson(v)});
const encodeForeignObject = dictEncodeJson => ({encodeJson: Data$dArgonaut$dEncode$dEncoders.encodeForeignObject(dictEncodeJson.encodeJson)});
export {
  encodeForeignObject,
  encodeIdentity,
  encodeJson,
  encodeJsonArray,
  encodeJsonChar,
  encodeJsonCodePoint,
  encodeJsonEither,
  encodeJsonInt,
  encodeJsonJBoolean,
  encodeJsonJNumber,
  encodeJsonJString,
  encodeJsonJson,
  encodeJsonList,
  encodeJsonMaybe,
  encodeJsonNonEmptyArray,
  encodeJsonNonEmptyList,
  encodeJsonNonEmpty_Array,
  encodeJsonNonEmpty_List,
  encodeJsonTuple,
  encodeJsonUnit,
  encodeMap,
  encodeNonEmptyString,
  encodeRecord,
  encodeSet,
  encodeVoid,
  gEncodeJson,
  gEncodeJsonCons,
  gEncodeJsonNil
};
