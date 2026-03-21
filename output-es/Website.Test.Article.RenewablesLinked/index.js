import * as Control$dPromise from "../Control.Promise/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Effect$dAff from "../Effect.Aff/index.js";
import * as Test$dUtil$dPuppeteer from "../Test.Util.Puppeteer/index.js";
const testFig = page => {
  const checkXTicks = Test$dUtil$dPuppeteer.waitFor("#fig-output > :nth-child(2) g.x-axis")(page);
  const checkPointRadius = Effect$dAff._bind(Test$dUtil$dPuppeteer.waitFor("#fig-output > :nth-child(2) circle.linechart-point")(page))(() => Test$dUtil$dPuppeteer.checkAttribute(page)("#fig-output > :nth-child(2) circle.linechart-point")("r")("2.0"));
  const clickBarChart = Effect$dAff._bind(Test$dUtil$dPuppeteer.waitFor("#fig-output > :nth-child(1) rect.bar")(page))(() => Effect$dAff._bind(Test$dUtil$dPuppeteer.click("#fig-output > :nth-child(1) rect.bar")(page))(() => Test$dUtil$dPuppeteer.checkAttribute(page)("#fig-output > :nth-child(1) rect.bar")("fill")("#57a157")));
  return Effect$dAff._bind(Test$dUtil$dPuppeteer.waitFor("#fig-output > :nth-child(1)")(page))(() => Effect$dAff._bind(Test$dUtil$dPuppeteer.waitFor("#fig-output > :nth-child(2)")(page))(() => Effect$dAff._bind(checkXTicks)(() => Effect$dAff._bind(checkPointRadius)(() => Effect$dAff._bind(Test$dUtil$dPuppeteer.clickToggle(page)("fig-data-pane"))(() => clickBarChart)))));
};
const main = /* #__PURE__ */ Control$dPromise.fromAff(/* #__PURE__ */ Data$dFoldable.traverse_(Effect$dAff.applicativeAff)(Data$dFoldable.foldableArray)(Data$dFoldable.identity)(/* #__PURE__ */ Test$dUtil$dPuppeteer.testURL("renewables-linked")([
  testFig
])));
export {main, testFig};
