import { runTests, testURL, waitFor } from "./shared/webtest-lib.js"

export const main = async () => {
   // don't seem to be able to combine these two steps in Firefox
   await runTests(testURL("")([ page => waitFor("#fig-output > div:nth-of-type(1) > svg")(page) ]))()
   await runTests(testURL("")([ page => waitFor("#fig-output > div:nth-of-type(2) > svg")(page) ]))()
   await runTests(testURL("convolution")([ page => waitFor("svg#fig-output")(page) ]))()
   await runTests(testURL("moving-average")([ page => waitFor("svg")(page) ]))()
}
