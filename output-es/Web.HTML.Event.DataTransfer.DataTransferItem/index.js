import * as $runtime from "../runtime.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNullable from "../Data.Nullable/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
import {_dataTransferItem, _kind, _length, type_} from "./foreign.js";
const $DataTransferItemKind = tag => tag;
const Text = /* #__PURE__ */ $DataTransferItemKind("Text");
const File = /* #__PURE__ */ $DataTransferItemKind("File");
const showDataTransferItemKind = {
  show: v => {
    if (v === "Text") { return "Text"; }
    if (v === "File") { return "File"; }
    $runtime.fail();
  }
};
const length = _length;
const kind = $0 => _kind(Data$dMaybe.Nothing, Data$dMaybe.Just, Text, File, $0);
const eqDataTransferItemKind = {
  eq: x => y => {
    if (x === "Text") { return y === "Text"; }
    return x === "File" && y === "File";
  }
};
const ordDataTransferItemKind = {
  compare: x => y => {
    if (x === "Text") {
      if (y === "Text") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "Text") { return Data$dOrdering.GT; }
    if (x === "File" && y === "File") { return Data$dOrdering.EQ; }
    $runtime.fail();
  },
  Eq0: () => eqDataTransferItemKind
};
const dataTransferItem = x => {
  const $0 = _dataTransferItem(x);
  return x$1 => Data$dNullable.nullable($0(x$1), Data$dMaybe.Nothing, Data$dMaybe.Just);
};
export {$DataTransferItemKind, File, Text, dataTransferItem, eqDataTransferItemKind, kind, length, ordDataTransferItemKind, showDataTransferItemKind};
export * from "./foreign.js";
