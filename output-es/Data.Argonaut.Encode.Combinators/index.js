// | Provides operators for a DSL to construct `Json` values:
// |
// | ```purs
// | myJson =
// |  "key1" := value1
// |    ~> "key2" :=? value2
// |    ~>? "key3" := value3
// |    ~> jsonEmptyObject
// | ```
import * as Data$dArgonaut$dEncode$dEncoders from "../Data.Argonaut.Encode.Encoders/index.js";
const extendOptional = dictEncodeJson => Data$dArgonaut$dEncode$dEncoders.extendOptional(dictEncodeJson.encodeJson);
const extend = dictEncodeJson => Data$dArgonaut$dEncode$dEncoders.extend(dictEncodeJson.encodeJson);
const assocOptional = dictEncodeJson => Data$dArgonaut$dEncode$dEncoders.assocOptional(dictEncodeJson.encodeJson);
const assoc = dictEncodeJson => Data$dArgonaut$dEncode$dEncoders.assoc(dictEncodeJson.encodeJson);
export {assoc, assocOptional, extend, extendOptional};
