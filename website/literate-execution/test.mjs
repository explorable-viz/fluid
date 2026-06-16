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

   // runs out of memory on Firefox
   // await testURL("evaluation-nap3/figure1", [
   //    async page => await waitFor(page, "#fig-output > svg:nth-of-type(1)"),
   //    async page => await waitFor(page, "#fig-output > svg:nth-of-type(2)"),
   // ])
}
