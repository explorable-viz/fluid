import * as $runtime from "../runtime.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNullable from "../Data.Nullable/index.js";
import * as Unsafe$dCoerce from "../Unsafe.Coerce/index.js";
import * as Web$dInternal$dFFI from "../Web.Internal.FFI/index.js";
import {
  _files,
  _form,
  _list,
  _setRangeText,
  accept,
  alt,
  autocomplete,
  autofocus,
  checkValidity,
  checked,
  defaultChecked,
  defaultValue,
  dirName,
  disabled,
  formAction,
  formEnctype,
  formMethod,
  formNoValidate,
  formTarget,
  height,
  indeterminate,
  labels,
  max,
  maxLength,
  min,
  minLength,
  multiple,
  name,
  pattern,
  placeholder,
  readOnly,
  reportValidity,
  required,
  select,
  selectionDirection,
  selectionEnd,
  selectionStart,
  setAccept,
  setAlt,
  setAutocomplete,
  setAutofocus,
  setChecked,
  setCustomValidity,
  setDefaultChecked,
  setDefaultValue,
  setDirName,
  setDisabled,
  setFormAction,
  setFormEnctype,
  setFormMethod,
  setFormNoValidate,
  setFormTarget,
  setHeight,
  setIndeterminate,
  setMax,
  setMaxLength,
  setMin,
  setMinLength,
  setMultiple,
  setName,
  setPattern,
  setPlaceholder,
  setRangeText,
  setReadOnly,
  setRequired,
  setSelectionDirection,
  setSelectionEnd,
  setSelectionRange,
  setSelectionStart,
  setSize,
  setSrc,
  setStep,
  setType,
  setValue,
  setValueAsDate,
  setValueAsNumber,
  setWidth,
  size,
  src,
  step,
  stepDownBy,
  stepUpBy,
  type_,
  validationMessage,
  validity,
  value,
  valueAsDate,
  valueAsNumber,
  width,
  willValidate
} from "./foreign.js";
const toParentNode = Unsafe$dCoerce.unsafeCoerce;
const toNonDocumentTypeChildNode = Unsafe$dCoerce.unsafeCoerce;
const toNode = Unsafe$dCoerce.unsafeCoerce;
const toHTMLElement = Unsafe$dCoerce.unsafeCoerce;
const toEventTarget = Unsafe$dCoerce.unsafeCoerce;
const toElement = Unsafe$dCoerce.unsafeCoerce;
const toChildNode = Unsafe$dCoerce.unsafeCoerce;
const stepUp$p = stepUpBy;
const stepUp = /* #__PURE__ */ stepUpBy(1);
const stepDown$p = stepDownBy;
const stepDown = /* #__PURE__ */ stepDownBy(1);
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
const list = x => {
  const $0 = _list(x);
  return () => {
    const a$p = $0();
    return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
  };
};
const fromParentNode = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLInputElement");
const fromNonDocumentTypeChildNode = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLInputElement");
const fromNode = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLInputElement");
const fromHTMLElement = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLInputElement");
const fromEventTarget = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLInputElement");
const fromElement = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLInputElement");
const fromChildNode = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLInputElement");
const form = x => {
  const $0 = _form(x);
  return () => {
    const a$p = $0();
    return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
  };
};
const files = x => {
  const $0 = _files(x);
  return () => {
    const a$p = $0();
    return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
  };
};
export {
  files,
  form,
  fromChildNode,
  fromElement,
  fromEventTarget,
  fromHTMLElement,
  fromNode,
  fromNonDocumentTypeChildNode,
  fromParentNode,
  list,
  setRangeText$p,
  stepDown,
  stepDown$p,
  stepUp,
  stepUp$p,
  toChildNode,
  toElement,
  toEventTarget,
  toHTMLElement,
  toNode,
  toNonDocumentTypeChildNode,
  toParentNode
};
export * from "./foreign.js";
