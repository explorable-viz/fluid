import * as $runtime from "../runtime.js";
import * as Data$dDate from "../Data.Date/index.js";
import * as Data$dDate$dComponent from "../Data.Date.Component/index.js";
import * as Data$dInt from "../Data.Int/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
const genDate = dictMonadGen => {
  const Monad0 = dictMonadGen.Monad0();
  const Bind1 = Monad0.Bind1();
  return Bind1.bind(dictMonadGen.Monad0().Bind1().Apply0().Functor0().map(x => {
    if (x >= -271820 && x <= 275759) { return x; }
    $runtime.fail();
  })(dictMonadGen.chooseInt(1900)(2100)))(year => Bind1.bind(Bind1.Apply0().Functor0().map(x => Data$dInt.toNumber(x))(dictMonadGen.chooseInt(0)(Data$dDate.isLeapYear(year)
    ? 365
    : 364)))(days => Monad0.Applicative0().pure((() => {
    const $0 = Data$dDate.exactDate(year)(Data$dDate$dComponent.January)(1);
    const $1 = (() => {
      if ($0.tag === "Just") { return Data$dDate.adjust(days)($0._1); }
      if ($0.tag === "Nothing") { return Data$dMaybe.Nothing; }
      $runtime.fail();
    })();
    if ($1.tag === "Just") { return $1._1; }
    $runtime.fail();
  })())));
};
export {genDate};
