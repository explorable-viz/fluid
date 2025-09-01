import { runTests, testURL, waitFor } from "./shared/webtest-lib.js"

export const main = async () => {
    await runTests(testURL("methane")([
        page => waitFor("#fig > svg:nth-of-type(1)")(page),
    ]))()
    console.log("Success!")
}
