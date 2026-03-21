import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Unsafe$dCoerce from "../Unsafe.Coerce/index.js";
import * as Web$dFile$dBlob from "../Web.File.Blob/index.js";
import {lastModified, name} from "./foreign.js";
const type_ = x => {
  const blobType = Web$dFile$dBlob.typeImpl(x);
  if (blobType === "") { return Data$dMaybe.Nothing; }
  return Data$dMaybe.$Maybe("Just", blobType);
};
const toBlob = Unsafe$dCoerce.unsafeCoerce;
const size = x => Web$dFile$dBlob.size(x);
export {size, toBlob, type_};
export * from "./foreign.js";
