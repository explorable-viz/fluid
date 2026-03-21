import * as Data$dEq from "../Data.Eq/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dShow from "../Data.Show/index.js";
const identity = x => x;
const Seconds = x => x;
const Minutes = x => x;
const Milliseconds = x => x;
const Hours = x => x;
const Days = x => x;
const toDuration = dict => dict.toDuration;
const showSeconds = {show: v => "(Seconds " + Data$dShow.showNumberImpl(v) + ")"};
const showMinutes = {show: v => "(Minutes " + Data$dShow.showNumberImpl(v) + ")"};
const showMilliseconds = {show: v => "(Milliseconds " + Data$dShow.showNumberImpl(v) + ")"};
const showHours = {show: v => "(Hours " + Data$dShow.showNumberImpl(v) + ")"};
const showDays = {show: v => "(Days " + Data$dShow.showNumberImpl(v) + ")"};
const semigroupSeconds = {append: v => v1 => v + v1};
const semigroupMinutes = {append: v => v1 => v + v1};
const semigroupMilliseconds = {append: v => v1 => v + v1};
const semigroupHours = {append: v => v1 => v + v1};
const semigroupDays = {append: v => v1 => v + v1};
const ordSeconds = Data$dOrd.ordNumber;
const ordMinutes = Data$dOrd.ordNumber;
const ordMilliseconds = Data$dOrd.ordNumber;
const ordHours = Data$dOrd.ordNumber;
const ordDays = Data$dOrd.ordNumber;
const newtypeSeconds = {Coercible0: () => {}};
const newtypeMinutes = {Coercible0: () => {}};
const newtypeMilliseconds = {Coercible0: () => {}};
const newtypeHours = {Coercible0: () => {}};
const newtypeDays = {Coercible0: () => {}};
const monoidSeconds = {mempty: 0.0, Semigroup0: () => semigroupSeconds};
const monoidMinutes = {mempty: 0.0, Semigroup0: () => semigroupMinutes};
const monoidMilliseconds = {mempty: 0.0, Semigroup0: () => semigroupMilliseconds};
const monoidHours = {mempty: 0.0, Semigroup0: () => semigroupHours};
const monoidDays = {mempty: 0.0, Semigroup0: () => semigroupDays};
const fromDuration = dict => dict.fromDuration;
const negateDuration = dictDuration => x => dictDuration.toDuration(-dictDuration.fromDuration(x));
const eqSeconds = Data$dEq.eqNumber;
const eqMinutes = Data$dEq.eqNumber;
const eqMilliseconds = Data$dEq.eqNumber;
const eqHours = Data$dEq.eqNumber;
const eqDays = Data$dEq.eqNumber;
const durationSeconds = {fromDuration: v => v * 1000.0, toDuration: v => v / 1000.0};
const durationMinutes = {fromDuration: v => v * 60000.0, toDuration: v => v / 60000.0};
const durationMilliseconds = {fromDuration: identity, toDuration: identity};
const durationHours = {fromDuration: v => v * 3600000.0, toDuration: v => v / 3600000.0};
const durationDays = {fromDuration: v => v * 86400000.0, toDuration: v => v / 86400000.0};
const convertDuration = dictDuration => dictDuration1 => x => dictDuration1.toDuration(dictDuration.fromDuration(x));
export {
  Days,
  Hours,
  Milliseconds,
  Minutes,
  Seconds,
  convertDuration,
  durationDays,
  durationHours,
  durationMilliseconds,
  durationMinutes,
  durationSeconds,
  eqDays,
  eqHours,
  eqMilliseconds,
  eqMinutes,
  eqSeconds,
  fromDuration,
  identity,
  monoidDays,
  monoidHours,
  monoidMilliseconds,
  monoidMinutes,
  monoidSeconds,
  negateDuration,
  newtypeDays,
  newtypeHours,
  newtypeMilliseconds,
  newtypeMinutes,
  newtypeSeconds,
  ordDays,
  ordHours,
  ordMilliseconds,
  ordMinutes,
  ordSeconds,
  semigroupDays,
  semigroupHours,
  semigroupMilliseconds,
  semigroupMinutes,
  semigroupSeconds,
  showDays,
  showHours,
  showMilliseconds,
  showMinutes,
  showSeconds,
  toDuration
};
