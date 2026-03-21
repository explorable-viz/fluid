import * as $runtime from "../runtime.js";
import * as Data$dDate$dComponent from "../Data.Date.Component/index.js";
import * as Data$dEnum$dGen from "../Data.Enum.Gen/index.js";
const genYear = dictMonadGen => dictMonadGen.Monad0().Bind1().Apply0().Functor0().map(x => {
  if (x >= -271820 && x <= 275759) { return x; }
  $runtime.fail();
})(dictMonadGen.chooseInt(1900)(2100));
const genWeekday = dictMonadGen => Data$dEnum$dGen.genBoundedEnum(dictMonadGen)(Data$dDate$dComponent.boundedEnumWeekday);
const genMonth = dictMonadGen => Data$dEnum$dGen.genBoundedEnum(dictMonadGen)(Data$dDate$dComponent.boundedEnumMonth);
const genDay = dictMonadGen => Data$dEnum$dGen.genBoundedEnum(dictMonadGen)(Data$dDate$dComponent.boundedEnumDay);
export {genDay, genMonth, genWeekday, genYear};
