import * as $runtime from "../runtime.js";
import * as Data$dArgonaut$dDecode$dDecoders from "../Data.Argonaut.Decode.Decoders/index.js";
import * as Data$dEither from "../Data.Either/index.js";
const getFieldOptional$p = dictDecodeJson => Data$dArgonaut$dDecode$dDecoders.getFieldOptional$p(dictDecodeJson.decodeJson);
const getFieldOptional = dictDecodeJson => Data$dArgonaut$dDecode$dDecoders.getFieldOptional(dictDecodeJson.decodeJson);
const getField = dictDecodeJson => Data$dArgonaut$dDecode$dDecoders.getField(dictDecodeJson.decodeJson);
const defaultField = parser => $$default => {
  if (parser.tag === "Left") { return Data$dEither.$Either("Left", parser._1); }
  if (parser.tag === "Right") {
    return Data$dEither.$Either(
      "Right",
      (() => {
        if (parser._1.tag === "Nothing") { return $$default; }
        if (parser._1.tag === "Just") { return parser._1._1; }
        $runtime.fail();
      })()
    );
  }
  $runtime.fail();
};
export {defaultField, getField, getFieldOptional, getFieldOptional$p};
