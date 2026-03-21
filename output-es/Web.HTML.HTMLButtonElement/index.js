import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNullable from "../Data.Nullable/index.js";
import * as Unsafe$dCoerce from "../Unsafe.Coerce/index.js";
import * as Web$dInternal$dFFI from "../Web.Internal.FFI/index.js";
import {
  _form,
  autofocus,
  checkValidity,
  disabled,
  formAction,
  formEnctype,
  formMethod,
  formNoValidate,
  formTarget,
  labels,
  name,
  reportValidity,
  setAutofocus,
  setCustomValidity,
  setDisabled,
  setFormAction,
  setFormEnctype,
  setFormMethod,
  setFormNoValidate,
  setFormTarget,
  setName,
  setType,
  setValue,
  type_,
  validationMessage,
  validity,
  value,
  willValidate
} from "./foreign.js";
const toParentNode = Unsafe$dCoerce.unsafeCoerce;
const toNonDocumentTypeChildNode = Unsafe$dCoerce.unsafeCoerce;
const toNode = Unsafe$dCoerce.unsafeCoerce;
const toHTMLElement = Unsafe$dCoerce.unsafeCoerce;
const toEventTarget = Unsafe$dCoerce.unsafeCoerce;
const toElement = Unsafe$dCoerce.unsafeCoerce;
const toChildNode = Unsafe$dCoerce.unsafeCoerce;
const fromParentNode = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLButtonElement");
const fromNonDocumentTypeChildNode = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLButtonElement");
const fromNode = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLButtonElement");
const fromHTMLElement = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLButtonElement");
const fromEventTarget = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLButtonElement");
const fromElement = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLButtonElement");
const fromChildNode = /* #__PURE__ */ Web$dInternal$dFFI.unsafeReadProtoTagged("HTMLButtonElement");
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
  toChildNode,
  toElement,
  toEventTarget,
  toHTMLElement,
  toNode,
  toNonDocumentTypeChildNode,
  toParentNode
};
export * from "./foreign.js";
