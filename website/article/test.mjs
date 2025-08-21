import { runTests, testURL, waitFor } from "./shared/webtest-lib.js"

export const main = async () => {
   await runTests(testURL("convolution")([ page => waitFor("svg#fig-output")(page) ]))()
   await runTests(testURL("moving-average")([ page => waitFor("svg")(page) ]))()
   await runTests(testURL("non-renewables")([
      page => waitFor("#fig-output > svg:nth-child(1)")(page),
      page => waitFor("#fig-output > svg:nth-child(2)")(page),
   ]))()
}
