import * as $runtime from "../runtime.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNullable from "../Data.Nullable/index.js";
import * as Unsafe$dCoerce from "../Unsafe.Coerce/index.js";
import * as Web$dInternal$dFFI from "../Web.Internal.FFI/index.js";
import {
  _form,
  _setRangeText,
  autocomplete,
  autofocus,
  checkValidity,
  cols,
  defaultValue,
  dirName,
  disabled,
  labels,
  maxLength,
  minLength,
  name,
  placeholder,
  readOnly,
  reportValidity,
  required,
  rows,
  select,
  selectionDirection,
  selectionEnd,
  selectionStart,
  setAutocomplete,
  setAutofocus,
  setCols,
  setCustomValidity,
  setDefaultValue,
  setDirName,
  setDisabled,
  setMaxLength,
  setMinLength,
  setName,
  setPlaceholder,
  setRangeText,
  setReadOnly,
  setRequired,
  setRows,
  setSelectionDirection,
  setSelectionEnd,
  setSelectionRange,
  setSelectionStart,
  setValue,
  setWrap,
  textLength,
  type_,
  validationMessage,
  validity,
  value,
  willValidate,
  wrap
} from "./foreign.js";
const toParentNode = Unsafe$dCoerce.unsafeCoerce;
const toNonDocumentTypeChildNode = Unsafe$dCoerce.unsafeCoerce;
const toNode = Unsafe$dCoerce.unsafeCoerce;
const toHTMLElement = Unsafe$dCoerce.unsafeCoerce;
const toEventTarget = Unsafe$dCoerce.unsafeCoerce;
const toElement = Unsafe$dCoerce.unsafeCoerce;
const toChildNode = Unsafe$dCoerce.unsafeCoerce;
const setRangeText$p = rpl => s => e => mode => area => {
  const $0 = (() => {
    if (mode === "Preserve") { return "preserve"; }
    if (mode === "Select") { return "select"; }
    if (mode === "Start") { return "start"; }
    if (mode === "End") { return "end"; }
    $runtime.fail();
  })();
  return () => _setRangeText(rpl, s, e, $0, area);
};
const fromParentNode = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLTextAreaElement");
const fromNonDocumentTypeChildNode = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLTextAreaElement");
const fromNode = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLTextAreaElement");
const fromHTMLElement = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLTextAreaElement");
const fromEventTarget = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLTextAreaElement");
const fromElement = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLTextAreaElement");
const fromChildNode = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLTextAreaElement");
const form = x => {
  const $0 = _form(x);
  return () => {
    const a$p = $0();
    return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
  };
};
export {
  form,
  fromChildNode,
  fromElement,
  fromEventTarget,
  fromHTMLElement,
  fromNode,
  fromNonDocumentTypeChildNode,
  fromParentNode,
  setRangeText$p,
  toChildNode,
  toElement,
  toEventTarget,
  toHTMLElement,
  toNode,
  toNonDocumentTypeChildNode,
  toParentNode
};
export * from "./foreign.js";
