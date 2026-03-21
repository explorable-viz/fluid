import * as Data$dRing from "../Data.Ring/index.js";
const recip = dict => dict.recip;
const rightDiv = dictDivisionRing => a => b => dictDivisionRing.Ring0().Semiring0().mul(a)(dictDivisionRing.recip(b));
const leftDiv = dictDivisionRing => a => b => dictDivisionRing.Ring0().Semiring0().mul(dictDivisionRing.recip(b))(a);
const divisionringNumber = {recip: x => 1.0 / x, Ring0: () => Data$dRing.ringNumber};
export {divisionringNumber, leftDiv, recip, rightDiv};
