import * as $runtime from "../runtime.js";
import * as Data$dEq from "../Data.Eq/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNullable from "../Data.Nullable/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Web$dEvent$dEventPhase from "../Web.Event.EventPhase/index.js";
import {_currentTarget, _target, bubbles, cancelable, defaultPrevented, eventPhaseIndex, preventDefault, stopImmediatePropagation, stopPropagation, timeStamp, type_} from "./foreign.js";
const EventType = x => x;
const target = x => Data$dNullable.nullable(_target(x), Data$dMaybe.Nothing, Data$dMaybe.Just);
const ordEventType = Data$dOrd.ordString;
const newtypeEventType = {Coercible0: () => {}};
const eventPhase = () => x => {
  const $0 = eventPhaseIndex(x);
  if ($0 === 0) { return Web$dEvent$dEventPhase.None; }
  if ($0 === 1) { return Web$dEvent$dEventPhase.Capturing; }
  if ($0 === 2) { return Web$dEvent$dEventPhase.AtTarget; }
  if ($0 === 3) { return Web$dEvent$dEventPhase.Bubbling; }
  $runtime.fail();
};
const eqEventType = Data$dEq.eqString;
const currentTarget = x => Data$dNullable.nullable(_currentTarget(x), Data$dMaybe.Nothing, Data$dMaybe.Just);
;
export * from "./foreign.js";
