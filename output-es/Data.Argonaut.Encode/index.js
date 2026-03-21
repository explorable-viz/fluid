import * as Data$dArgonaut$dCore from "../Data.Argonaut.Core/index.js";
const toJsonString = dictEncodeJson => x => Data$dArgonaut$dCore.stringify(dictEncodeJson.encodeJson(x));
export {toJsonString};
