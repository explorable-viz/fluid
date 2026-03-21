import * as $runtime from "../runtime.js";
import {appendCopyMode, copyFile_EXCL, copyFile_FICLONE, copyFile_FICLONE_FORCE, f_OK, r_OK, w_OK, x_OK} from "./foreign.js";
const $FileFlags = tag => tag;
const R = /* #__PURE__ */ $FileFlags("R");
const R_PLUS = /* #__PURE__ */ $FileFlags("R_PLUS");
const RS = /* #__PURE__ */ $FileFlags("RS");
const RS_PLUS = /* #__PURE__ */ $FileFlags("RS_PLUS");
const W = /* #__PURE__ */ $FileFlags("W");
const WX = /* #__PURE__ */ $FileFlags("WX");
const W_PLUS = /* #__PURE__ */ $FileFlags("W_PLUS");
const WX_PLUS = /* #__PURE__ */ $FileFlags("WX_PLUS");
const A = /* #__PURE__ */ $FileFlags("A");
const AX = /* #__PURE__ */ $FileFlags("AX");
const A_PLUS = /* #__PURE__ */ $FileFlags("A_PLUS");
const AX_PLUS = /* #__PURE__ */ $FileFlags("AX_PLUS");
const semigroupCopyMode = {append: l => r => appendCopyMode(l, r)};
const showFileFlags = {
  show: v => {
    if (v === "R") { return "R"; }
    if (v === "R_PLUS") { return "R_PLUS"; }
    if (v === "RS") { return "RS"; }
    if (v === "RS_PLUS") { return "RS_PLUS"; }
    if (v === "W") { return "W"; }
    if (v === "WX") { return "WX"; }
    if (v === "W_PLUS") { return "W_PLUS"; }
    if (v === "WX_PLUS") { return "WX_PLUS"; }
    if (v === "A") { return "A"; }
    if (v === "AX") { return "AX"; }
    if (v === "A_PLUS") { return "A_PLUS"; }
    if (v === "AX_PLUS") { return "AX_PLUS"; }
    $runtime.fail();
  }
};
const fileFlagsToNode = ff => {
  if (ff === "R") { return "r"; }
  if (ff === "R_PLUS") { return "r+"; }
  if (ff === "RS") { return "rs"; }
  if (ff === "RS_PLUS") { return "rs+"; }
  if (ff === "W") { return "w"; }
  if (ff === "WX") { return "wx"; }
  if (ff === "W_PLUS") { return "w+"; }
  if (ff === "WX_PLUS") { return "wx+"; }
  if (ff === "A") { return "a"; }
  if (ff === "AX") { return "ax"; }
  if (ff === "A_PLUS") { return "a+"; }
  if (ff === "AX_PLUS") { return "ax+"; }
  $runtime.fail();
};
const eqFileFlags = {
  eq: x => y => (() => {
    if (x === "R") { return "R"; }
    if (x === "R_PLUS") { return "R_PLUS"; }
    if (x === "RS") { return "RS"; }
    if (x === "RS_PLUS") { return "RS_PLUS"; }
    if (x === "W") { return "W"; }
    if (x === "WX") { return "WX"; }
    if (x === "W_PLUS") { return "W_PLUS"; }
    if (x === "WX_PLUS") { return "WX_PLUS"; }
    if (x === "A") { return "A"; }
    if (x === "AX") { return "AX"; }
    if (x === "A_PLUS") { return "A_PLUS"; }
    if (x === "AX_PLUS") { return "AX_PLUS"; }
    $runtime.fail();
  })() === (() => {
    if (y === "R") { return "R"; }
    if (y === "R_PLUS") { return "R_PLUS"; }
    if (y === "RS") { return "RS"; }
    if (y === "RS_PLUS") { return "RS_PLUS"; }
    if (y === "W") { return "W"; }
    if (y === "WX") { return "WX"; }
    if (y === "W_PLUS") { return "W_PLUS"; }
    if (y === "WX_PLUS") { return "WX_PLUS"; }
    if (y === "A") { return "A"; }
    if (y === "AX") { return "AX"; }
    if (y === "A_PLUS") { return "A_PLUS"; }
    if (y === "AX_PLUS") { return "AX_PLUS"; }
    $runtime.fail();
  })()
};
const defaultCopyMode = copyFile_EXCL;
const defaultAccessMode = f_OK;
export {
  $FileFlags,
  A,
  AX,
  AX_PLUS,
  A_PLUS,
  R,
  RS,
  RS_PLUS,
  R_PLUS,
  W,
  WX,
  WX_PLUS,
  W_PLUS,
  defaultAccessMode,
  defaultCopyMode,
  eqFileFlags,
  fileFlagsToNode,
  semigroupCopyMode,
  showFileFlags
};
export * from "./foreign.js";
