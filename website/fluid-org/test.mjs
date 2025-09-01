import { runTests, testURL, waitFor } from "./shared/webtest-lib.js"

export const main = async () => {
   await runTests(testURL("convolution")([ page => waitFor("svg#fig-output")(page) ]))()
}
