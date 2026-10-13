import { testURL, waitFor } from "@fluid-org/fluid/script/webtest-lib.mjs"

export const main = async () => {
   await testURL("ar6-wg1/spm/figure4b", [
      async page => await waitFor(page, "#fig-output > svg:nth-of-type(1)"),
      async page => await waitFor(page, "#fig-output > svg:nth-of-type(2)"),
   ])

   await testURL("ar6-wg1/spm/table1", [
      async page => await waitFor(page, "#fig-output > div:nth-of-type(1).para-text"),
      async page => await waitFor(page, "#fig-output > div:nth-of-type(2).para-text"),
   ])

   await testURL("matrix-multiply", [
      async page => await waitFor(page, "#fig-output > svg:nth-of-type(1)"),
      async page => await waitFor(page, "#fig-output > svg:nth-of-type(2)"),
   ])

   await testURL("ar6-wg1/ts/figure12", [
      async page => await waitFor(page, "#fig > svg:nth-of-type(1)"),
   ])

   // one load: Firefox doesn't finish a second load of this page in the same browser
   await testURL("evaluation-nap3/figure1", [
      async page => {
         await waitFor(page, "#fig-output > svg:nth-of-type(1)")
         await waitFor(page, "#fig-output > svg:nth-of-type(2)")
      },
   ], { timeout: 120000 })
}
