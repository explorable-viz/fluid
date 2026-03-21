import * as Data$dTime$dDuration from "../Data.Time.Duration/index.js";
const genSeconds = dictMonadGen => dictMonadGen.Monad0().Bind1().Apply0().Functor0().map(Data$dTime$dDuration.Seconds)(dictMonadGen.chooseFloat(0.0)(600.0));
const genMinutes = dictMonadGen => dictMonadGen.Monad0().Bind1().Apply0().Functor0().map(Data$dTime$dDuration.Minutes)(dictMonadGen.chooseFloat(0.0)(600.0));
const genMilliseconds = dictMonadGen => dictMonadGen.Monad0().Bind1().Apply0().Functor0().map(Data$dTime$dDuration.Milliseconds)(dictMonadGen.chooseFloat(0.0)(600000.0));
const genHours = dictMonadGen => dictMonadGen.Monad0().Bind1().Apply0().Functor0().map(Data$dTime$dDuration.Hours)(dictMonadGen.chooseFloat(0.0)(240.0));
const genDays = dictMonadGen => dictMonadGen.Monad0().Bind1().Apply0().Functor0().map(Data$dTime$dDuration.Days)(dictMonadGen.chooseFloat(0.0)(42.0));
export {genDays, genHours, genMilliseconds, genMinutes, genSeconds};
