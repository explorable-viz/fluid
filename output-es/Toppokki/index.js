import * as Control$dPromise from "../Control.Promise/index.js";
import * as Effect$dAff from "../Effect.Aff/index.js";
import * as Effect$dUncurried from "../Effect.Uncurried/index.js";
import * as Unsafe$dCoerce from "../Unsafe.Coerce/index.js";
import {
  _bringToFront,
  _click,
  _close,
  _content,
  _focus,
  _getLocationHref,
  _goto,
  _keyboardDown,
  _keyboardPress,
  _keyboardSendCharacter,
  _keyboardType,
  _keyboardUp,
  _launch,
  _launchChromeAWS,
  _newPage,
  _on,
  _pageWaitForSelector,
  _pdf,
  _screenshot,
  _select,
  _setUserAgent,
  _setViewport,
  _type,
  _unsafeEvaluateOnNewDocument,
  _unsafeEvaluateStringFunction,
  _unsafePageEval,
  _unsafePageEvalAll,
  _waitForNavigation,
  puppeteer
} from "./foreign.js";
const URL = x => x;
const Selector = x => x;
const KeyboardKey = x => x;
const runPromiseAffE4 = f => a => b => c => d => Effect$dAff._bind(Effect$dAff._liftEffect(f(a, b, c, d)))(Control$dPromise.toAff$p(Control$dPromise.coerce));
const type_ = () => runPromiseAffE4(_type);
const runPromiseAffE3 = f => a => b => c => Effect$dAff._bind(Effect$dAff._liftEffect(f(a, b, c)))(Control$dPromise.toAff$p(Control$dPromise.coerce));
const select = /* #__PURE__ */ runPromiseAffE3(_select);
const unsafePageEval = /* #__PURE__ */ runPromiseAffE3(_unsafePageEval);
const unsafePageEvalAll = /* #__PURE__ */ runPromiseAffE3(_unsafePageEvalAll);
const runPromiseAffE2 = f => a => b => Effect$dAff._bind(Effect$dAff._liftEffect(f(a, b)))(Control$dPromise.toAff$p(Control$dPromise.coerce));
const screenshot = () => o => p => Effect$dAff._bind(Effect$dAff._liftEffect(_screenshot(o, p)))(Control$dPromise.toAff$p(Control$dPromise.coerce));
const setUserAgent = /* #__PURE__ */ runPromiseAffE2(_setUserAgent);
const setViewport = /* #__PURE__ */ runPromiseAffE2(_setViewport);
const unsafeEvaluateOnNewDocument = /* #__PURE__ */ runPromiseAffE2(_unsafeEvaluateOnNewDocument);
const unsafeEvaluateStringFunction = /* #__PURE__ */ runPromiseAffE2(_unsafeEvaluateStringFunction);
const waitForNavigation = () => runPromiseAffE2(_waitForNavigation);
const runPromiseAffE1 = f => a => Effect$dAff._bind(Effect$dAff._liftEffect(f(a)))(Control$dPromise.toAff$p(Control$dPromise.coerce));
const pdf = () => runPromiseAffE2(_pdf);
const pageWaitForSelector = () => runPromiseAffE3(_pageWaitForSelector);
const onPageError = /* #__PURE__ */ Effect$dUncurried.runEffectFn3(_on)("pageerror");
const onLoad = /* #__PURE__ */ Effect$dUncurried.runEffectFn3(_on)("load");
const newtypeURL = {Coercible0: () => {}};
const newtypeSelector = {Coercible0: () => {}};
const newPage = /* #__PURE__ */ runPromiseAffE1(_newPage);
const networkIdle2 = "networkidle2";
const networkIdle0 = "networkidle0";
const networkIdle = "networkidle";
const makePDFMargin = () => Unsafe$dCoerce.unsafeCoerce;
const launchChromeAWS = () => runPromiseAffE2(_launchChromeAWS);
const launch = () => runPromiseAffE1(_launch);
const keyboardUp = () => runPromiseAffE3(_keyboardUp);
const keyboardType = () => runPromiseAffE3(_keyboardType);
const keyboardSendCharacter = /* #__PURE__ */ runPromiseAffE2(_keyboardSendCharacter);
const keyboardPress = () => runPromiseAffE3(_keyboardPress);
const keyboardDown = () => runPromiseAffE3(_keyboardDown);
const $$goto = /* #__PURE__ */ runPromiseAffE2(_goto);
const getLocationRef = p => Effect$dAff._bind(Effect$dAff._liftEffect(_getLocationHref(p)))(Control$dPromise.toAff$p(Control$dPromise.coerce));
const focus = /* #__PURE__ */ runPromiseAffE2(_focus);
const content = /* #__PURE__ */ runPromiseAffE1(_content);
const close = /* #__PURE__ */ runPromiseAffE1(_close);
const click = /* #__PURE__ */ runPromiseAffE2(_click);
const bringToFront = /* #__PURE__ */ runPromiseAffE1(_bringToFront);
export {
  KeyboardKey,
  Selector,
  URL,
  bringToFront,
  click,
  close,
  content,
  focus,
  getLocationRef,
  $$goto as goto,
  keyboardDown,
  keyboardPress,
  keyboardSendCharacter,
  keyboardType,
  keyboardUp,
  launch,
  launchChromeAWS,
  makePDFMargin,
  networkIdle,
  networkIdle0,
  networkIdle2,
  newPage,
  newtypeSelector,
  newtypeURL,
  onLoad,
  onPageError,
  pageWaitForSelector,
  pdf,
  runPromiseAffE1,
  runPromiseAffE2,
  runPromiseAffE3,
  runPromiseAffE4,
  screenshot,
  select,
  setUserAgent,
  setViewport,
  type_,
  unsafeEvaluateOnNewDocument,
  unsafeEvaluateStringFunction,
  unsafePageEval,
  unsafePageEvalAll,
  waitForNavigation
};
export * from "./foreign.js";
