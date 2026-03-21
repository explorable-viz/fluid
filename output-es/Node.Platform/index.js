// | This module defines data type for the different platforms supported by
// | Node.js
import * as $runtime from "../runtime.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
const $Platform = tag => tag;
const AIX = /* #__PURE__ */ $Platform("AIX");
const Darwin = /* #__PURE__ */ $Platform("Darwin");
const FreeBSD = /* #__PURE__ */ $Platform("FreeBSD");
const Linux = /* #__PURE__ */ $Platform("Linux");
const OpenBSD = /* #__PURE__ */ $Platform("OpenBSD");
const SunOS = /* #__PURE__ */ $Platform("SunOS");
const Win32 = /* #__PURE__ */ $Platform("Win32");
const Android = /* #__PURE__ */ $Platform("Android");
const toString = v => {
  if (v === "AIX") { return "aix"; }
  if (v === "Darwin") { return "darwin"; }
  if (v === "FreeBSD") { return "freebsd"; }
  if (v === "Linux") { return "linux"; }
  if (v === "OpenBSD") { return "openbsd"; }
  if (v === "SunOS") { return "sunos"; }
  if (v === "Win32") { return "win32"; }
  if (v === "Android") { return "android"; }
  $runtime.fail();
};
const showPlatform = {
  show: v => {
    if (v === "AIX") { return "AIX"; }
    if (v === "Darwin") { return "Darwin"; }
    if (v === "FreeBSD") { return "FreeBSD"; }
    if (v === "Linux") { return "Linux"; }
    if (v === "OpenBSD") { return "OpenBSD"; }
    if (v === "SunOS") { return "SunOS"; }
    if (v === "Win32") { return "Win32"; }
    if (v === "Android") { return "Android"; }
    $runtime.fail();
  }
};
const fromString = v => {
  if (v === "aix") { return Data$dMaybe.$Maybe("Just", AIX); }
  if (v === "darwin") { return Data$dMaybe.$Maybe("Just", Darwin); }
  if (v === "freebsd") { return Data$dMaybe.$Maybe("Just", FreeBSD); }
  if (v === "linux") { return Data$dMaybe.$Maybe("Just", Linux); }
  if (v === "openbsd") { return Data$dMaybe.$Maybe("Just", OpenBSD); }
  if (v === "sunos") { return Data$dMaybe.$Maybe("Just", SunOS); }
  if (v === "win32") { return Data$dMaybe.$Maybe("Just", Win32); }
  if (v === "android") { return Data$dMaybe.$Maybe("Just", Android); }
  return Data$dMaybe.Nothing;
};
const eqPlatform = {
  eq: x => y => {
    if (x === "AIX") { return y === "AIX"; }
    if (x === "Darwin") { return y === "Darwin"; }
    if (x === "FreeBSD") { return y === "FreeBSD"; }
    if (x === "Linux") { return y === "Linux"; }
    if (x === "OpenBSD") { return y === "OpenBSD"; }
    if (x === "SunOS") { return y === "SunOS"; }
    if (x === "Win32") { return y === "Win32"; }
    return x === "Android" && y === "Android";
  }
};
const ordPlatform = {
  compare: x => y => {
    if (x === "AIX") {
      if (y === "AIX") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "AIX") { return Data$dOrdering.GT; }
    if (x === "Darwin") {
      if (y === "Darwin") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "Darwin") { return Data$dOrdering.GT; }
    if (x === "FreeBSD") {
      if (y === "FreeBSD") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "FreeBSD") { return Data$dOrdering.GT; }
    if (x === "Linux") {
      if (y === "Linux") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "Linux") { return Data$dOrdering.GT; }
    if (x === "OpenBSD") {
      if (y === "OpenBSD") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "OpenBSD") { return Data$dOrdering.GT; }
    if (x === "SunOS") {
      if (y === "SunOS") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "SunOS") { return Data$dOrdering.GT; }
    if (x === "Win32") {
      if (y === "Win32") { return Data$dOrdering.EQ; }
      return Data$dOrdering.LT;
    }
    if (y === "Win32") { return Data$dOrdering.GT; }
    if (x === "Android" && y === "Android") { return Data$dOrdering.EQ; }
    $runtime.fail();
  },
  Eq0: () => eqPlatform
};
export {$Platform, AIX, Android, Darwin, FreeBSD, Linux, OpenBSD, SunOS, Win32, eqPlatform, fromString, ordPlatform, showPlatform, toString};
