import { runTests, testURL, waitFor } from "./shared/webtest-lib.js"

export const main = async () => {
    await runTests(testURL("ar6-wg1/spm/figure4b")([
        page => waitFor("#fig-output > div:nth-of-type(1)")(page),
        page => waitFor("#fig-output > div:nth-of-type(2)")(page),
    ]))()
    await runTests(testURL("ar6-wg1/spm/table1")([
        page => waitFor("#fig-output > div:nth-of-type(1) > div.para-text")(page),
        page => waitFor("#fig-output > div:nth-of-type(2) > div.para-text")(page)
    ]))()
    console.log("Success!")
}
