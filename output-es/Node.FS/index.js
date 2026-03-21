import * as $runtime from "../runtime.js";
const $SymlinkType = tag => tag;
const FileLink = /* #__PURE__ */ $SymlinkType("FileLink");
const DirLink = /* #__PURE__ */ $SymlinkType("DirLink");
const JunctionLink = /* #__PURE__ */ $SymlinkType("JunctionLink");
const symlinkTypeToNode = ty => {
  if (ty === "FileLink") { return "file"; }
  if (ty === "DirLink") { return "dir"; }
  if (ty === "JunctionLink") { return "junction"; }
  $runtime.fail();
};
const showSymlinkType = {
  show: v => {
    if (v === "FileLink") { return "FileLink"; }
    if (v === "DirLink") { return "DirLink"; }
    if (v === "JunctionLink") { return "JunctionLink"; }
    $runtime.fail();
  }
};
const eqSymlinkType = {
  eq: x => y => {
    if (x === "FileLink") { return y === "FileLink"; }
    if (x === "DirLink") { return y === "DirLink"; }
    return x === "JunctionLink" && y === "JunctionLink";
  }
};
export {$SymlinkType, DirLink, FileLink, JunctionLink, eqSymlinkType, showSymlinkType, symlinkTypeToNode};
