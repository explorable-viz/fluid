import * as $runtime from "../runtime.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
const $NodeType = tag => tag;
const ElementNode = /* #__PURE__ */ $NodeType("ElementNode");
const AttributeNode = /* #__PURE__ */ $NodeType("AttributeNode");
const TextNode = /* #__PURE__ */ $NodeType("TextNode");
const CDATASectionNode = /* #__PURE__ */ $NodeType("CDATASectionNode");
const EntityReferenceNode = /* #__PURE__ */ $NodeType("EntityReferenceNode");
const EntityNode = /* #__PURE__ */ $NodeType("EntityNode");
const ProcessingInstructionNode = /* #__PURE__ */ $NodeType("ProcessingInstructionNode");
const CommentNode = /* #__PURE__ */ $NodeType("CommentNode");
const DocumentNode = /* #__PURE__ */ $NodeType("DocumentNode");
const DocumentTypeNode = /* #__PURE__ */ $NodeType("DocumentTypeNode");
const DocumentFragmentNode = /* #__PURE__ */ $NodeType("DocumentFragmentNode");
const NotationNode = /* #__PURE__ */ $NodeType("NotationNode");
const toEnumNodeType = v => {
  if (v === 1) { return Data$dMaybe.$Maybe("Just", ElementNode); }
  if (v === 2) { return Data$dMaybe.$Maybe("Just", AttributeNode); }
  if (v === 3) { return Data$dMaybe.$Maybe("Just", TextNode); }
  if (v === 4) { return Data$dMaybe.$Maybe("Just", CDATASectionNode); }
  if (v === 5) { return Data$dMaybe.$Maybe("Just", EntityReferenceNode); }
  if (v === 6) { return Data$dMaybe.$Maybe("Just", EntityNode); }
  if (v === 7) { return Data$dMaybe.$Maybe("Just", ProcessingInstructionNode); }
  if (v === 8) { return Data$dMaybe.$Maybe("Just", CommentNode); }
  if (v === 9) { return Data$dMaybe.$Maybe("Just", DocumentNode); }
  if (v === 10) { return Data$dMaybe.$Maybe("Just", DocumentTypeNode); }
  if (v === 11) { return Data$dMaybe.$Maybe("Just", DocumentFragmentNode); }
  if (v === 12) { return Data$dMaybe.$Maybe("Just", NotationNode); }
  return Data$dMaybe.Nothing;
};
const showNodeType = {
  show: v => {
    if (v === "ElementNode") { return "ElementNode"; }
    if (v === "AttributeNode") { return "AttributeNode"; }
    if (v === "TextNode") { return "TextNode"; }
    if (v === "CDATASectionNode") { return "CDATASectionNode"; }
    if (v === "EntityReferenceNode") { return "EntityReferenceNode"; }
    if (v === "EntityNode") { return "EntityNode"; }
    if (v === "ProcessingInstructionNode") { return "ProcessingInstructionNode"; }
    if (v === "CommentNode") { return "CommentNode"; }
    if (v === "DocumentNode") { return "DocumentNode"; }
    if (v === "DocumentTypeNode") { return "DocumentTypeNode"; }
    if (v === "DocumentFragmentNode") { return "DocumentFragmentNode"; }
    if (v === "NotationNode") { return "NotationNode"; }
    $runtime.fail();
  }
};
const fromEnumNodeType = v => {
  if (v === "ElementNode") { return 1; }
  if (v === "AttributeNode") { return 2; }
  if (v === "TextNode") { return 3; }
  if (v === "CDATASectionNode") { return 4; }
  if (v === "EntityReferenceNode") { return 5; }
  if (v === "EntityNode") { return 6; }
  if (v === "ProcessingInstructionNode") { return 7; }
  if (v === "CommentNode") { return 8; }
  if (v === "DocumentNode") { return 9; }
  if (v === "DocumentTypeNode") { return 10; }
  if (v === "DocumentFragmentNode") { return 11; }
  if (v === "NotationNode") { return 12; }
  $runtime.fail();
};
const eqNodeType = {
  eq: x => y => {
    if (x === "ElementNode") { return y === "ElementNode"; }
    if (x === "AttributeNode") { return y === "AttributeNode"; }
    if (x === "TextNode") { return y === "TextNode"; }
    if (x === "CDATASectionNode") { return y === "CDATASectionNode"; }
    if (x === "EntityReferenceNode") { return y === "EntityReferenceNode"; }
    if (x === "EntityNode") { return y === "EntityNode"; }
    if (x === "ProcessingInstructionNode") { return y === "ProcessingInstructionNode"; }
    if (x === "CommentNode") { return y === "CommentNode"; }
    if (x === "DocumentNode") { return y === "DocumentNode"; }
    if (x === "DocumentTypeNode") { return y === "DocumentTypeNode"; }
    if (x === "DocumentFragmentNode") { return y === "DocumentFragmentNode"; }
    return x === "NotationNode" && y === "NotationNode";
  }
};
const ordNodeType = {
  compare: x => y => Data$dOrd.ordInt.compare((() => {
    if (x === "ElementNode") { return 1; }
    if (x === "AttributeNode") { return 2; }
    if (x === "TextNode") { return 3; }
    if (x === "CDATASectionNode") { return 4; }
    if (x === "EntityReferenceNode") { return 5; }
    if (x === "EntityNode") { return 6; }
    if (x === "ProcessingInstructionNode") { return 7; }
    if (x === "CommentNode") { return 8; }
    if (x === "DocumentNode") { return 9; }
    if (x === "DocumentTypeNode") { return 10; }
    if (x === "DocumentFragmentNode") { return 11; }
    if (x === "NotationNode") { return 12; }
    $runtime.fail();
  })())((() => {
    if (y === "ElementNode") { return 1; }
    if (y === "AttributeNode") { return 2; }
    if (y === "TextNode") { return 3; }
    if (y === "CDATASectionNode") { return 4; }
    if (y === "EntityReferenceNode") { return 5; }
    if (y === "EntityNode") { return 6; }
    if (y === "ProcessingInstructionNode") { return 7; }
    if (y === "CommentNode") { return 8; }
    if (y === "DocumentNode") { return 9; }
    if (y === "DocumentTypeNode") { return 10; }
    if (y === "DocumentFragmentNode") { return 11; }
    if (y === "NotationNode") { return 12; }
    $runtime.fail();
  })()),
  Eq0: () => eqNodeType
};
const enumNodeType = {
  succ: a => {
    const $0 = (() => {
      if (a === "ElementNode") { return 2; }
      if (a === "AttributeNode") { return 3; }
      if (a === "TextNode") { return 4; }
      if (a === "CDATASectionNode") { return 5; }
      if (a === "EntityReferenceNode") { return 6; }
      if (a === "EntityNode") { return 7; }
      if (a === "ProcessingInstructionNode") { return 8; }
      if (a === "CommentNode") { return 9; }
      if (a === "DocumentNode") { return 10; }
      if (a === "DocumentTypeNode") { return 11; }
      if (a === "DocumentFragmentNode") { return 12; }
      if (a === "NotationNode") { return 13; }
      $runtime.fail();
    })();
    if ($0 === 1) { return Data$dMaybe.$Maybe("Just", ElementNode); }
    if ($0 === 2) { return Data$dMaybe.$Maybe("Just", AttributeNode); }
    if ($0 === 3) { return Data$dMaybe.$Maybe("Just", TextNode); }
    if ($0 === 4) { return Data$dMaybe.$Maybe("Just", CDATASectionNode); }
    if ($0 === 5) { return Data$dMaybe.$Maybe("Just", EntityReferenceNode); }
    if ($0 === 6) { return Data$dMaybe.$Maybe("Just", EntityNode); }
    if ($0 === 7) { return Data$dMaybe.$Maybe("Just", ProcessingInstructionNode); }
    if ($0 === 8) { return Data$dMaybe.$Maybe("Just", CommentNode); }
    if ($0 === 9) { return Data$dMaybe.$Maybe("Just", DocumentNode); }
    if ($0 === 10) { return Data$dMaybe.$Maybe("Just", DocumentTypeNode); }
    if ($0 === 11) { return Data$dMaybe.$Maybe("Just", DocumentFragmentNode); }
    if ($0 === 12) { return Data$dMaybe.$Maybe("Just", NotationNode); }
    return Data$dMaybe.Nothing;
  },
  pred: a => {
    const $0 = (() => {
      if (a === "ElementNode") { return 0; }
      if (a === "AttributeNode") { return 1; }
      if (a === "TextNode") { return 2; }
      if (a === "CDATASectionNode") { return 3; }
      if (a === "EntityReferenceNode") { return 4; }
      if (a === "EntityNode") { return 5; }
      if (a === "ProcessingInstructionNode") { return 6; }
      if (a === "CommentNode") { return 7; }
      if (a === "DocumentNode") { return 8; }
      if (a === "DocumentTypeNode") { return 9; }
      if (a === "DocumentFragmentNode") { return 10; }
      if (a === "NotationNode") { return 11; }
      $runtime.fail();
    })();
    if ($0 === 1) { return Data$dMaybe.$Maybe("Just", ElementNode); }
    if ($0 === 2) { return Data$dMaybe.$Maybe("Just", AttributeNode); }
    if ($0 === 3) { return Data$dMaybe.$Maybe("Just", TextNode); }
    if ($0 === 4) { return Data$dMaybe.$Maybe("Just", CDATASectionNode); }
    if ($0 === 5) { return Data$dMaybe.$Maybe("Just", EntityReferenceNode); }
    if ($0 === 6) { return Data$dMaybe.$Maybe("Just", EntityNode); }
    if ($0 === 7) { return Data$dMaybe.$Maybe("Just", ProcessingInstructionNode); }
    if ($0 === 8) { return Data$dMaybe.$Maybe("Just", CommentNode); }
    if ($0 === 9) { return Data$dMaybe.$Maybe("Just", DocumentNode); }
    if ($0 === 10) { return Data$dMaybe.$Maybe("Just", DocumentTypeNode); }
    if ($0 === 11) { return Data$dMaybe.$Maybe("Just", DocumentFragmentNode); }
    if ($0 === 12) { return Data$dMaybe.$Maybe("Just", NotationNode); }
    return Data$dMaybe.Nothing;
  },
  Ord0: () => ordNodeType
};
const boundedNodeType = {bottom: ElementNode, top: NotationNode, Ord0: () => ordNodeType};
const boundedEnumNodeType = {cardinality: 12, toEnum: toEnumNodeType, fromEnum: fromEnumNodeType, Bounded0: () => boundedNodeType, Enum1: () => enumNodeType};
export {
  $NodeType,
  AttributeNode,
  CDATASectionNode,
  CommentNode,
  DocumentFragmentNode,
  DocumentNode,
  DocumentTypeNode,
  ElementNode,
  EntityNode,
  EntityReferenceNode,
  NotationNode,
  ProcessingInstructionNode,
  TextNode,
  boundedEnumNodeType,
  boundedNodeType,
  enumNodeType,
  eqNodeType,
  fromEnumNodeType,
  ordNodeType,
  showNodeType,
  toEnumNodeType
};
