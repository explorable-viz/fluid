import { runTests, testURL, waitFor } from "./shared/webtest-lib.js"

export const main = async () => {
  await runTests(testURL("ar6-wg1/spm/figure4b")([
    page => waitFor("#fig-output > svg:nth-of-type(1)")(page),
    page => waitFor("#fig-output > svg:nth-of-type(2)")(page),
  ]))()
  await runTests(testURL("ar6-wg1/spm/table1")([
    page => waitFor("#fig-output > div:nth-of-type(1).para-text")(page),
    page => waitFor("#fig-output > div:nth-of-type(2).para-text")(page)
  ]))()
  await runTests(testURL("matrix-multiply")([
    page => waitFor("#fig-output > svg:nth-of-type(1)")(page),
    page => waitFor("#fig-output > svg:nth-of-type(2)")(page),
  ]))()
  /*
  //TEST 1: TS index page has a link to Figure 12
  await runTests(testURL("ar6-wg1/ts")([
    page => waitFor("a[href='figure12']")(page),
  ]))()
  //TEST 2: TS Figure 12 page has figure container
  await runTests(testURL("ar6-wg1/ts/figure12")([
    page => waitFor("#fig")(page),
  ]))()
  */
  console.log("Success!")
}
