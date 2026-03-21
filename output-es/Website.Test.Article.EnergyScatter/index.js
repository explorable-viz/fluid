import * as Control$dPromise from "../Control.Promise/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Effect$dAff from "../Effect.Aff/index.js";
import * as Test$dUtil$dPuppeteer from "../Test.Util.Puppeteer/index.js";
const testFig = page => {
  const clickScatterPlotPoint = Effect$dAff._bind(Test$dUtil$dPuppeteer.waitFor("div#fig .scatterplot-point")(page))(() => Effect$dAff._bind(Test$dUtil$dPuppeteer.click("div#fig .scatterplot-point")(page))(() => Effect$dAff._bind(Test$dUtil$dPuppeteer.checkAttributeContains(page)("div#fig .scatterplot-point")("class")("selected-primary-persistent"))(() => Effect$dAff._bind(Test$dUtil$dPuppeteer.checkAttribute(page)("div#fig .scatterplot-point")("r")("3.2"))(() => Test$dUtil$dPuppeteer.checkTextContent(page)("div#fig-input-renewables > div.table-caption")("renewables (40 of 240 × 5 of 5)")))));
  return Effect$dAff._bind(Test$dUtil$dPuppeteer.waitFor("svg")(page))(() => Effect$dAff._bind(Test$dUtil$dPuppeteer.clickToggle(page)("fig-data-pane"))(() => clickScatterPlotPoint));
};
const main = /* #__PURE__ */ Control$dPromise.fromAff(/* #__PURE__ */ Data$dFoldable.traverse_(Effect$dAff.applicativeAff)(Data$dFoldable.foldableArray)(Data$dFoldable.identity)(/* #__PURE__ */ Test$dUtil$dPuppeteer.testURL("energy-scatter")([
  testFig
])));
export {main, testFig};
