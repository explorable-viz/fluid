import * as $runtime from "../runtime.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNullable from "../Data.Nullable/index.js";
import {_attributeName, _attributeNamespace, _nextSibling, _oldValue, _previousSibling, addedNodes, removedNodes, target, typeString} from "./foreign.js";
const $MutationRecordType = tag => tag;
const MutationRecordAttributes = /* #__PURE__ */ $MutationRecordType("MutationRecordAttributes");
const MutationRecordCharacterData = /* #__PURE__ */ $MutationRecordType("MutationRecordCharacterData");
const MutationRecordChildList = /* #__PURE__ */ $MutationRecordType("MutationRecordChildList");
const type_ = () => x => {
  const $0 = typeString(x);
  return () => {
    const a$p = $0();
    if (a$p === "attributes") { return MutationRecordAttributes; }
    if (a$p === "characterData") { return MutationRecordCharacterData; }
    if (a$p === "childList") { return MutationRecordChildList; }
    $runtime.fail();
  };
};
const previousSibling = x => {
  const $0 = _previousSibling(x);
  return () => {
    const a$p = $0();
    return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
  };
};
const oldValue = x => {
  const $0 = _oldValue(x);
  return () => {
    const a$p = $0();
    return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
  };
};
const nextSibling = x => {
  const $0 = _nextSibling(x);
  return () => {
    const a$p = $0();
    return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
  };
};
const attributeNamespace = x => {
  const $0 = _attributeNamespace(x);
  return () => {
    const a$p = $0();
    return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
  };
};
const attributeName = x => {
  const $0 = _attributeName(x);
  return () => {
    const a$p = $0();
    return Data$dNullable.nullable(a$p, Data$dMaybe.Nothing, Data$dMaybe.Just);
  };
};
export {
  $MutationRecordType,
  MutationRecordAttributes,
  MutationRecordCharacterData,
  MutationRecordChildList,
  attributeName,
  attributeNamespace,
  nextSibling,
  oldValue,
  previousSibling,
  type_
};
export * from "./foreign.js";
