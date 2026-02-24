import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
import * as Data$dShow from "../Data.Show/index.js";
const $ResponseHeader = (_1, _2) => ({tag: "ResponseHeader", _1, _2});
const ResponseHeader = value0 => value1 => $ResponseHeader(value0, value1);
const value = v => v._2;
const showResponseHeader = {show: v => "(ResponseHeader " + Data$dShow.showStringImpl(v._1) + " " + Data$dShow.showStringImpl(v._2) + ")"};
const name = v => v._1;
const eqResponseHeader = {eq: x => y => x._1 === y._1 && x._2 === y._2};
const ordResponseHeader = {
  compare: x => y => {
    const v = Data$dOrd.ordString.compare(x._1)(y._1);
    if (v === "LT") { return Data$dOrdering.LT; }
    if (v === "GT") { return Data$dOrdering.GT; }
    return Data$dOrd.ordString.compare(x._2)(y._2);
  },
  Eq0: () => eqResponseHeader
};
export { ResponseHeader,     };
