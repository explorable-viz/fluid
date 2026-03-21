import * as Data$dDate$dGen from "../Data.Date.Gen/index.js";
import * as Data$dDateTime from "../Data.DateTime/index.js";
import * as Data$dTime$dGen from "../Data.Time.Gen/index.js";
const genDateTime = dictMonadGen => {
  const Apply0 = dictMonadGen.Monad0().Bind1().Apply0();
  return Apply0.apply(Apply0.Functor0().map(Data$dDateTime.DateTime)(Data$dDate$dGen.genDate(dictMonadGen)))(Data$dTime$dGen.genTime(dictMonadGen));
};
export {genDateTime};
