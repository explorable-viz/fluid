// | ## Handling events emitted by an `EventEmitter`
// |
// | One can add callbacks to an `EventEmitter` on two major axes:
// | - whether listener is added to the **end** (i.e. `on`) or **start** (i.e. `prependListener`) of the array
// | - whether a listener is automatically removed after the first event (i.e. `once` or `prependOnceListener`).
// |
// | This module provides functions for each of the above 4 callback-adding functions
// | If `<fn>` is either `on`, `once`, `prependListener`, or `prependOnceListener`, then this module exposes
// | 1. `<fn>` - returns a callback that removes the listener
// | 2. `<fn>_` - does not return a callback that can remove the listener
// |
// | ## Defining events emitted by an `EventEmitter`
// |
// | Below, we'll provide an example for how to define an event handler for a type. Let's assume the following:
// | - There is a type `Foo` that exends `EventEmitter`
// | - `Foo` values can handle "bar" events
// | - a "bar" event takes the following callback: `EffectFn2 (Nullable Error) String Unit`
// | - the `String` value is always either "red", "green", or "blue"
// |
// | Then we would write
// | ```
// | data Color 
// |   = Red 
// |   | Green 
// |   | Blue
// |
// | -- Note: see docs on `EventHandle` 
// | -- for the below naming convention justification 
// | -- of suffixing an event name with `H`.
// | barH 
// |   :: EventHandle 
// |        Foo 
// |        (Maybe Error -> Color -> Effect Unit) 
// |        (EffectFn1 (Nullable Error) String Unit)
// | barH = EventHandle "bar" $ \psCb -> 
// |   mkEffectFn2 \nullableError str ->
// |     psCb (toMaybe nullableError) case str of
// |       "red" -> Red
// |       "green" -> Green
// |       "blue" -> Blue
// |       _ -> 
// |         unsafeCrashWith $ 
// |           "Impossible String value for event 'bar': " <> show str
// | ```
// |
// | ## Emitting events via an `EventEmitter`
// |
// | Unfortunately, there isn't a good way to emit events safely in PureScript. If one wants to emit an event
// | in PureScript code that will be consumed by PureScript code, there are better abstractions to use than `EventEmitter`.
// | If one wants to emit an event in PureScript code that will be consumed by JavaScript code, then
// | the `unsafeEmitFn` function can be used to call n-ary functions. However, this is very unsafe. See its docs for more context.
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Effect$dUncurried from "../Effect.Uncurried/index.js";
import {
  new as $$new,
  eventNamesImpl,
  getMaxListenersImpl,
  listenerCountImpl,
  setMaxListenersImpl,
  symbolOrStr,
  unsafeEmitFn,
  unsafeOff,
  unsafeOn,
  unsafeOnce,
  unsafePrependListener,
  unsafePrependOnceListener
} from "./foreign.js";
const $EventHandle = (_1, _2) => ({tag: "EventHandle", _1, _2});
const EventHandle = value0 => value1 => $EventHandle(value0, value1);
const subscribeSameFunction = (onXFn, eventEmitter, eventName, jsCb) => {
  onXFn(eventEmitter, eventName, jsCb);
  return () => unsafeOff(eventEmitter, eventName, jsCb);
};
const setMaxListeners = max => emitter => () => setMaxListenersImpl(emitter, max);
const setUnlimitedListeners = /* #__PURE__ */ setMaxListeners(0);
const removeListenerH = /* #__PURE__ */ $EventHandle("removeListener", cb => jsSymbol => cb(symbolOrStr(Data$dEither.Left, Data$dEither.Right, jsSymbol))());
const prependOnceListener_ = v => psCb => eventEmitter => {
  const $0 = v._1;
  const $1 = v._2(psCb);
  return () => unsafePrependOnceListener(eventEmitter, $0, $1);
};
const prependOnceListener = v => psCb => eventEmitter => {
  const $0 = v._1;
  const $1 = v._2(psCb);
  return () => subscribeSameFunction(unsafePrependOnceListener, eventEmitter, $0, $1);
};
const prependListener_ = v => psCb => eventEmitter => {
  const $0 = v._1;
  const $1 = v._2(psCb);
  return () => unsafePrependListener(eventEmitter, $0, $1);
};
const prependListener = v => psCb => eventEmitter => {
  const $0 = v._1;
  const $1 = v._2(psCb);
  return () => subscribeSameFunction(unsafePrependListener, eventEmitter, $0, $1);
};
const once_ = v => psCb => eventEmitter => {
  const $0 = v._1;
  const $1 = v._2(psCb);
  return () => unsafeOnce(eventEmitter, $0, $1);
};
const once = v => psCb => eventEmitter => {
  const $0 = v._1;
  const $1 = v._2(psCb);
  return () => subscribeSameFunction(unsafeOnce, eventEmitter, $0, $1);
};
const on_ = v => psCb => eventEmitter => {
  const $0 = v._1;
  const $1 = v._2(psCb);
  return () => unsafeOn(eventEmitter, $0, $1);
};
const on = v => psCb => eventEmitter => {
  const $0 = v._1;
  const $1 = v._2(psCb);
  return () => subscribeSameFunction(unsafeOn, eventEmitter, $0, $1);
};
const newListenerH = /* #__PURE__ */ $EventHandle("newListener", cb => jsSymbol => cb(symbolOrStr(Data$dEither.Left, Data$dEither.Right, jsSymbol))());
const listenerCount = emitter => eventName => () => listenerCountImpl(emitter, eventName);
const getMaxListeners = /* #__PURE__ */ Effect$dUncurried.runEffectFn1(getMaxListenersImpl);
const eventNames = ee => Data$dFunctor.arrayMap(x => symbolOrStr(Data$dEither.Left, Data$dEither.Right, x))(eventNamesImpl(ee));
export {
  $EventHandle,
  EventHandle,
  eventNames,
  getMaxListeners,
  listenerCount,
  newListenerH,
  on,
  on_,
  once,
  once_,
  prependListener,
  prependListener_,
  prependOnceListener,
  prependOnceListener_,
  removeListenerH,
  setMaxListeners,
  setUnlimitedListeners,
  subscribeSameFunction
};
export * from "./foreign.js";
