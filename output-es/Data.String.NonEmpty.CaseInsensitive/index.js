import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dString$dCommon from "../Data.String.Common/index.js";
const CaseInsensitiveNonEmptyString = x => x;
const showCaseInsensitiveNonEmptyString = {show: v => "(CaseInsensitiveNonEmptyString (NonEmptyString.unsafeFromString " + Data$dShow.showStringImpl(v) + "))"};
const newtypeCaseInsensitiveNonEmptyString = {Coercible0: () => {}};
const eqCaseInsensitiveNonEmptyString = {eq: v => v1 => Data$dString$dCommon.toLower(v) === Data$dString$dCommon.toLower(v1)};
const ordCaseInsensitiveNonEmptyString = {
  compare: v => v1 => Data$dOrd.ordString.compare(Data$dString$dCommon.toLower(v))(Data$dString$dCommon.toLower(v1)),
  Eq0: () => eqCaseInsensitiveNonEmptyString
};
export {CaseInsensitiveNonEmptyString, eqCaseInsensitiveNonEmptyString, newtypeCaseInsensitiveNonEmptyString, ordCaseInsensitiveNonEmptyString, showCaseInsensitiveNonEmptyString};
