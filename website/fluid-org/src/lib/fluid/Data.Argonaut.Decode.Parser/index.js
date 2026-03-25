import * as $runtime from "../runtime.js";
import * as Data$dArgonaut$dDecode$dError from "../Data.Argonaut.Decode.Error/index.js";
import * as Data$dArgonaut$dParser from "../Data.Argonaut.Parser/index.js";
import * as Data$dEither from "../Data.Either/index.js";
const parseJson = x => {
  const $0 = Data$dArgonaut$dParser._jsonParser(Data$dEither.Left, Data$dEither.Right, x);
  if ($0.tag === "Left") { return Data$dEither.$Either("Left", Data$dArgonaut$dDecode$dError.$JsonDecodeError("TypeMismatch", "JSON")); }
  if ($0.tag === "Right") { return Data$dEither.$Either("Right", $0._1); }
  $runtime.fail();
};
export {parseJson};
