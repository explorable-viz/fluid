import * as $runtime from "../runtime.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
import * as Data$dShow from "../Data.Show/index.js";
const $RequestHeader = (tag, _1, _2) => ({tag, _1, _2});
const Accept = value0 => $RequestHeader("Accept", value0);
const ContentType = value0 => $RequestHeader("ContentType", value0);
const RequestHeader = value0 => value1 => $RequestHeader("RequestHeader", value0, value1);
const value = v => {
  if (v.tag === "Accept") { return v._1; }
  if (v.tag === "ContentType") { return v._1; }
  if (v.tag === "RequestHeader") { return v._2; }
  $runtime.fail();
};
const showRequestHeader = {
  show: v => {
    if (v.tag === "Accept") { return "(Accept (MediaType " + Data$dShow.showStringImpl(v._1) + "))"; }
    if (v.tag === "ContentType") { return "(ContentType (MediaType " + Data$dShow.showStringImpl(v._1) + "))"; }
    if (v.tag === "RequestHeader") { return "(RequestHeader " + Data$dShow.showStringImpl(v._1) + " " + Data$dShow.showStringImpl(v._2) + ")"; }
    $runtime.fail();
  }
};
const name = v => {
  if (v.tag === "Accept") { return "Accept"; }
  if (v.tag === "ContentType") { return "Content-Type"; }
  if (v.tag === "RequestHeader") { return v._1; }
  $runtime.fail();
};
const eqRequestHeader = {
  eq: x => y => {
    if (x.tag === "Accept") { return y.tag === "Accept" && x._1 === y._1; }
    if (x.tag === "ContentType") { return y.tag === "ContentType" && x._1 === y._1; }
    return x.tag === "RequestHeader" && y.tag === "RequestHeader" && x._1 === y._1 && x._2 === y._2;
  }
};
const ordRequestHeader = {
  compare: x => y => {
    if (x.tag === "Accept") {
      if (y.tag === "Accept") { return Data$dOrd.ordString.compare(x._1)(y._1); }
      return Data$dOrdering.LT;
    }
    if (y.tag === "Accept") { return Data$dOrdering.GT; }
    if (x.tag === "ContentType") {
      if (y.tag === "ContentType") { return Data$dOrd.ordString.compare(x._1)(y._1); }
      return Data$dOrdering.LT;
    }
    if (y.tag === "ContentType") { return Data$dOrdering.GT; }
    if (x.tag === "RequestHeader" && y.tag === "RequestHeader") {
      const v = Data$dOrd.ordString.compare(x._1)(y._1);
      if (v === "LT") { return Data$dOrdering.LT; }
      if (v === "GT") { return Data$dOrdering.GT; }
      return Data$dOrd.ordString.compare(x._2)(y._2);
    }
    $runtime.fail();
  },
  Eq0: () => eqRequestHeader
};
export {$RequestHeader,        };
