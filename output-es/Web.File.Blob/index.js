import * as Data$dInt from "../Data.Int/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNumber from "../Data.Number/index.js";
import {blobImpl, size, slice, typeImpl} from "./foreign.js";
const EndByte = x => x;
const StartByte = x => x;
const type_ = blob => {
  const blobType = typeImpl(blob);
  if (blobType === "") { return Data$dMaybe.Nothing; }
  return Data$dMaybe.$Maybe("Just", blobType);
};
const slice$p = /* #__PURE__ */ slice("");
const idxFromNumber = x => Data$dNumber.round(x);
const idxFromInt = x => Data$dInt.toNumber(x);
const fromString = str => ct => blobImpl([str])(ct);
const fromArray = args => opts => blobImpl(args)(opts);
export {EndByte, StartByte, fromArray, fromString, idxFromInt, idxFromNumber, slice$p, type_};
export * from "./foreign.js";
