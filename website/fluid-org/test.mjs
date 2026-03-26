import {
   checkAlignment, checkWidthApprox,
   testURL, waitFor
} from "@explorable-viz/fluid/script/webtest-lib.mjs"

export const main = async () => {
   await testURL("", [
      async page => {
         await waitFor(page, ".site-header")
         await waitFor(page, ".grid-container")
         await waitFor(page, "h3.title")

         // Header nav left edge aligns with text content left edge
         await checkAlignment(page, ".header-nav", ".grid-container .flex-left-align", "left")

         // Text pane is approximately 800px wide
         await checkWidthApprox(page, ".grid-container .flex-left-align", 800)
      }
   ])

   await testURL("faq", [
      async page => await waitFor(page, ".grid-container")
   ])

   await testURL("research", [
      async page => await waitFor(page, ".grid-container")
   ])

   await testURL("supporters", [
      async page => await waitFor(page, ".grid-container")
   ])
}
