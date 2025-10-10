import { runTests, testURL, waitFor } from "./shared/webtest-lib.js"

export const main = async () => {
   await runTests(testURL("convolution")([ page => waitFor("svg#fig-output")(page) ]))()
   const point = "div#fig .scatterplot-point"
   await runTests(testURL("methane")([ page => waitFor("#fig svg:nth-of-type(1)")(page) ]))()
   await runTests(testURL("moving-average")([ page => waitFor("svg")(page) ]))()
   await runTests(testURL("non-renewables")([
      page => waitFor("#fig-output > svg:nth-of-type(1)")(page),
      page => waitFor("#fig-output > svg:nth-of-type(2)")(page),
   ]))()
   await runTests(testURL("renewables-linked")([page => waitFor("#fig-output > svg:nth-of-type(1)")(page)]))()
   await runTests(testURL("scigen-1805.02474v1-10")([ page => waitFor("div#fig-output")(page) ]))()
}
