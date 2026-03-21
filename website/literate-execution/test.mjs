import { runTests, testURL, waitFor } from "../../fluid/dist/fluid/shared/webtest-lib.js"

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
  await runTests(testURL("ar6-wg1/ts/figure12")([
    page => waitFor("#fig > svg:nth-of-type(1)")(page),
  ]))()
  // runs out of memory on Firefox
  // await runTests(testURL("evaluation-nap3/figure1")([
  //   page => waitFor("#fig-output > svg:nth-of-type(1)")(page),
  //   page => waitFor("#fig-output > svg:nth-of-type(2)")(page),
  // ]))()
  console.log("Success!")
}
