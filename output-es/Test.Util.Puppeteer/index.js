import * as Control$dPromise from "../Control.Promise/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dString$dCodeUnits from "../Data.String.CodeUnits/index.js";
import * as Effect$dAff from "../Effect.Aff/index.js";
import * as Effect$dConsole from "../Effect.Console/index.js";
import * as Effect$dException from "../Effect.Exception/index.js";
import * as Test$dUtil from "../Test.Util/index.js";
import * as Toppokki from "../Toppokki/index.js";
import {_launch} from "./foreign.js";
const sequence_ = /* #__PURE__ */ Data$dFoldable.traverse_(Effect$dAff.applicativeAff)(Data$dFoldable.foldableArray)(Data$dFoldable.identity);
const testCondition = /* #__PURE__ */ Test$dUtil.testCondition(Effect$dAff.monadThrowAff)(Effect$dAff.monadEffectAff);
const for_ = /* #__PURE__ */ Data$dFoldable.for_(Effect$dAff.applicativeAff)(Data$dFoldable.foldableArray);
const timeout = 60000;
const textContentValue = page => selector => Effect$dAff._bind(Toppokki.runPromiseAffE3(Toppokki._unsafePageEval)(selector)("element => element.textContent")(page))(captionText => Effect$dAff._pure(captionText));
const show$p = v => v;
const runTests = arr => Control$dPromise.fromAff(sequence_(arr));
const puppeteerTests = {logging: true, headless: true};
const log$p = dictMonadEffect => x => dictMonadEffect.liftEffect(Effect$dConsole.log(x));
const waitFor = selector => page => {
  const report = testCondition(selector);
  return Effect$dAff._bind(Effect$dAff._liftEffect(Effect$dConsole.log("Waiting for " + selector)))(() => Effect$dAff._catchError(Effect$dAff._bind(Effect$dAff._map(v => {})(Toppokki.runPromiseAffE3(Toppokki._pageWaitForSelector)(selector)({
    timeout: 60000,
    visible: true
  })(page)))(() => Effect$dAff._bind(Effect$dAff._liftEffect(Effect$dConsole.log("-> found")))(() => report(true)("exists"))))(e => report(false)(Effect$dException.showErrorImpl(e))));
};
const waitForHidden = selector => page => Effect$dAff._bind(Effect$dAff._liftEffect(Effect$dConsole.log("Waiting for " + selector)))(() => Effect$dAff._bind(Effect$dAff._map(v => {})(Toppokki.runPromiseAffE3(Toppokki._pageWaitForSelector)(selector)({
  timeout: 60000,
  visible: false
})(page)))(() => Effect$dAff._liftEffect(Effect$dConsole.log("-> found"))));
const launch = /* #__PURE__ */ Toppokki.runPromiseAffE1(_launch);
const $$goto = v => page => Effect$dAff._bind(Effect$dAff._liftEffect(Effect$dConsole.log("Going to " + Data$dShow.showStringImpl(v))))(() => Effect$dAff._bind(Effect$dAff._liftEffect(Toppokki._goto(
  v,
  page
)))(Control$dPromise.toAff$p(Control$dPromise.coerce)));
const getAttributeValue = page => selector => attribute => Effect$dAff._bind(Toppokki.runPromiseAffE3(Toppokki._unsafePageEval)(selector)("element => element.getAttribute('" + attribute + "')")(page))(attrValue => Effect$dAff._pure(attrValue));
const defaultViewport = {deviceScaleFactor: 1.0, hasTouch: false, height: 800.0, isLandscape: false, isMobile: false, width: 1200.0};
const click = element => page => Effect$dAff._bind(Effect$dAff._bind(Effect$dAff._liftEffect(Toppokki._click(element, page)))(Control$dPromise.toAff$p(Control$dPromise.coerce)))(() => testCondition(element)(true)("click"));
const clickToggle = page => idPrev => Effect$dAff._bind(waitFor("#grid.data-pane-hidden")(page))(() => {
  const toggle = "div#" + idPrev + " + div";
  return Effect$dAff._bind(waitFor(toggle)(page))(() => Effect$dAff._bind(click(toggle)(page))(() => waitFor("#grid:not(.data-pane-hidden)")(page)));
});
const checkTextContent = page => selector => expected => Effect$dAff._bind(waitFor(selector)(page))(() => Effect$dAff._bind(textContentValue(page)(selector))(text => Effect$dAff._bind(testCondition(selector)(text === expected)("text == " + Data$dShow.showStringImpl(expected)))(() => Effect$dAff._pure())));
const checkAttributeContains = page => sel => attr => expected => Effect$dAff._bind(getAttributeValue(page)(sel)(attr))(found => {
  const success = Data$dString$dCodeUnits.contains(expected)(found);
  return testCondition(sel)(success)(attr + " contains " + Data$dShow.showStringImpl(expected) + (success ? "" : " (got " + found + ")"));
});
const checkAttribute = page => sel => attr => expected => Effect$dAff._bind(getAttributeValue(page)(sel)(attr))(found => testCondition(sel)(found === expected)(attr + " == " + Data$dShow.showStringImpl(expected) + (found === expected
  ? ""
  : " (got \"" + found + "\")")));
const browserTests = suffix => browserName => launchBrowser => tests => Effect$dAff._bind(Effect$dAff._liftEffect(Effect$dConsole.log("browserTests: " + browserName)))(() => Effect$dAff._bind(launchBrowser)(browser => Effect$dAff._bind(Effect$dAff._bind(Effect$dAff._liftEffect(Toppokki._newPage(browser)))(Control$dPromise.toAff$p(Control$dPromise.coerce)))(page => {
  const url = "http://127.0.0.1:8080/" + suffix;
  return Effect$dAff._bind(for_(tests)(test => Effect$dAff._bind($$goto(url)(page))(() => test(page))))(() => Effect$dAff._bind(Effect$dAff._liftEffect(Toppokki._close(browser)))(Control$dPromise.toAff$p(Control$dPromise.coerce)));
})));
const testURL = suffix => tests => {
  const testOn = browser => browserTests(suffix)(browser)(Effect$dAff._bind(Effect$dAff._liftEffect(_launch({browser, defaultViewport, headless: true})))(Control$dPromise.toAff$p(Control$dPromise.coerce)))(tests);
  return [testOn("chrome"), testOn("firefox")];
};
export {
  browserTests,
  checkAttribute,
  checkAttributeContains,
  checkTextContent,
  click,
  clickToggle,
  defaultViewport,
  for_,
  getAttributeValue,
  $$goto as goto,
  launch,
  log$p,
  puppeteerTests,
  runTests,
  sequence_,
  show$p,
  testCondition,
  testURL,
  textContentValue,
  timeout,
  waitFor,
  waitForHidden
};
export * from "./foreign.js";
