import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dString$dCommon from "../Data.String.Common/index.js";
const CaseInsensitiveString = x => x;
const showCaseInsensitiveString = {show: v => "(CaseInsensitiveString " + Data$dShow.showStringImpl(v) + ")"};
const newtypeCaseInsensitiveString = {Coercible0: () => {}};
const eqCaseInsensitiveString = {eq: v => v1 => Data$dString$dCommon.toLower(v) === Data$dString$dCommon.toLower(v1)};
const ordCaseInsensitiveString = {
  compare: v => v1 => Data$dOrd.ordString.compare(Data$dString$dCommon.toLower(v))(Data$dString$dCommon.toLower(v1)),
  Eq0: () => eqCaseInsensitiveString
};
export {CaseInsensitiveString, eqCaseInsensitiveString, newtypeCaseInsensitiveString, ordCaseInsensitiveString, showCaseInsensitiveString};
