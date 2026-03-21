import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dShow from "../Data.Show/index.js";
const StatusCode = x => x;
const showStatusCode = {show: v => "(StatusCode " + Data$dShow.showIntImpl(v) + ")"};
const newtypeStatusCode = {Coercible0: () => {}};
const eqStatusCode = {eq: x => y => x === y};
const ordStatusCode = {compare: x => y => Data$dOrd.ordInt.compare(x)(y), Eq0: () => eqStatusCode};
export {StatusCode, eqStatusCode, newtypeStatusCode, ordStatusCode, showStatusCode};
