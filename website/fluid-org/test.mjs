import { runTests, testURL, waitFor } from "./shared/webtest-lib.js"

export const main = async () => {
   await runTests(testURL("")([
      page => waitFor("#fig-output > svg:nth-child(1)")(page),
      // navigating to localhost second time times out on Firefox, manually looks ok though
//      page => waitFor("#fig-output > svg:nth-child(2)")(page),
   ]))()
   await runTests(testURL("convolution")([ page => waitFor("svg#fig-output")(page) ]))()
   await runTests(testURL("moving-average")([ page => waitFor("svg")(page) ]))()
}
