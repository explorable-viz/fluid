import * as Data$dEnum$dGen from "../Data.Enum.Gen/index.js";
import * as Data$dTime from "../Data.Time/index.js";
import * as Data$dTime$dComponent from "../Data.Time.Component/index.js";
const genTime = dictMonadGen => {
  const Apply0 = dictMonadGen.Monad0().Bind1().Apply0();
  return Apply0.apply(Apply0.apply(Apply0.apply(Apply0.Functor0().map(Data$dTime.Time)(Data$dEnum$dGen.genBoundedEnum(dictMonadGen)(Data$dTime$dComponent.boundedEnumHour)))(Data$dEnum$dGen.genBoundedEnum(dictMonadGen)(Data$dTime$dComponent.boundedEnumMinute)))(Data$dEnum$dGen.genBoundedEnum(dictMonadGen)(Data$dTime$dComponent.boundedEnumSecond)))(Data$dEnum$dGen.genBoundedEnum(dictMonadGen)(Data$dTime$dComponent.boundedEnumMillisecond));
};
export {genTime};
