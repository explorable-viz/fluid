import * as $runtime from "../runtime.js";
import * as Data$dArgonaut$dDecode$dParser from "../Data.Argonaut.Decode.Parser/index.js";
import * as Data$dEither from "../Data.Either/index.js";
const fromJsonString = dictDecodeJson => a => {
  const $0 = Data$dArgonaut$dDecode$dParser.parseJson(a);
  if ($0.tag === "Left") { return Data$dEither.$Either("Left", $0._1); }
  if ($0.tag === "Right") { return dictDecodeJson.decodeJson($0._1); }
  $runtime.fail();
};
export {fromJsonString};
