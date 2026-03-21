import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dShow from "../Data.Show/index.js";
const MediaType = x => x;
const showMediaType = {show: v => "(MediaType " + Data$dShow.showStringImpl(v) + ")"};
const newtypeMediaType = {Coercible0: () => {}};
const eqMediaType = {eq: x => y => x === y};
const ordMediaType = {compare: x => y => Data$dOrd.ordString.compare(x)(y), Eq0: () => eqMediaType};
export {MediaType, eqMediaType, newtypeMediaType, ordMediaType, showMediaType};
