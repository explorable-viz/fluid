import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNullable from "../Data.Nullable/index.js";
import * as Unsafe$dCoerce from "../Unsafe.Coerce/index.js";
import {
  _offsetParent,
  _read,
  blur,
  click,
  contentEditable,
  dir,
  draggable,
  focus,
  hidden,
  isContentEditable,
  lang,
  offsetHeight,
  offsetLeft,
  offsetTop,
  offsetWidth,
  setContentEditable,
  setDir,
  setDraggable,
  setHidden,
  setLang,
  setSpellcheck,
  setTabIndex,
  setTitle,
  spellcheck,
  tabIndex,
  title
} from "./foreign.js";
const toParentNode = Unsafe$dCoerce.unsafeCoerce;
const toNonDocumentTypeChildNode = Unsafe$dCoerce.unsafeCoerce;
const toNode = Unsafe$dCoerce.unsafeCoerce;
const toEventTarget = Unsafe$dCoerce.unsafeCoerce;
const toElement = Unsafe$dCoerce.unsafeCoerce;
const toChildNode = Unsafe$dCoerce.unsafeCoerce;
const offsetParent = x => {
  const $0 = _offsetParent(x);
  return () => {
    const a$p = $0();
    return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
  };
};
const fromParentNode = x => _read(Data$dMaybe.Nothing, Data$dMaybe.Just, x);
const fromNonDocumentTypeChildNode = x => _read(Data$dMaybe.Nothing, Data$dMaybe.Just, x);
const fromNode = x => _read(Data$dMaybe.Nothing, Data$dMaybe.Just, x);
const fromEventTarget = x => _read(Data$dMaybe.Nothing, Data$dMaybe.Just, x);
const fromElement = x => _read(Data$dMaybe.Nothing, Data$dMaybe.Just, x);
const fromChildNode = x => _read(Data$dMaybe.Nothing, Data$dMaybe.Just, x);
export {
  fromChildNode,
  fromElement,
  fromEventTarget,
  fromNode,
  fromNonDocumentTypeChildNode,
  fromParentNode,
  offsetParent,
  toChildNode,
  toElement,
  toEventTarget,
  toNode,
  toNonDocumentTypeChildNode,
  toParentNode
};
export * from "./foreign.js";
