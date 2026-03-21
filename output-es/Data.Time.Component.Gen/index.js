import * as Data$dEnum$dGen from "../Data.Enum.Gen/index.js";
import * as Data$dTime$dComponent from "../Data.Time.Component/index.js";
const genSecond = dictMonadGen => Data$dEnum$dGen.genBoundedEnum(dictMonadGen)(Data$dTime$dComponent.boundedEnumSecond);
const genMinute = dictMonadGen => Data$dEnum$dGen.genBoundedEnum(dictMonadGen)(Data$dTime$dComponent.boundedEnumMinute);
const genMillisecond = dictMonadGen => Data$dEnum$dGen.genBoundedEnum(dictMonadGen)(Data$dTime$dComponent.boundedEnumMillisecond);
const genHour = dictMonadGen => Data$dEnum$dGen.genBoundedEnum(dictMonadGen)(Data$dTime$dComponent.boundedEnumHour);
export {genHour, genMillisecond, genMinute, genSecond};
