import { runTests, testURL, waitFor } from "./shared/webtest-lib.js"

export const main = async () => {
    await runTests(testURL("figure-spm-4b")([
        page => waitFor("#fig-output > svg:nth-of-type(1)")(page),
        page => waitFor("#fig-output > svg:nth-of-type(2)")(page),
    ]))()
    await runTests(testURL("table-spm-1")([
        page => waitFor("#fig-output > div.para-text:nth-of-type(1)")(page),
        page => waitFor("#fig-output > div.para-text:nth-of-type(2)")(page)
    ]))()
    console.log("Success!")
}
