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
  // Test A: TS index page contains link to Figure 12
  await runTests(testURL("ar6-wg1/ts")([
    page => waitFor('a[href="figure12"]')(page),
  ]))()
  console.log("Success!")
  // Test B: evaluation-nap3 index page contains link to Figure 1
  await runTests(testURL("evaluation-nap3")([
    page => waitFor('a[href="figure1"]')(page)
  ]))()
}
