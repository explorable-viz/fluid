import {
   checkAlignment, checkWidthApprox,
   testMobile, testURL, waitFor
} from "@explorable-viz/fluid/script/webtest-lib.mjs"

async function checkHeaderAlignment(page) {
   await waitFor(page, ".site-header")
   await waitFor(page, ".grid-container")
   await checkAlignment(page, ".header-nav", ".grid-container .flex-left-align", "left")
}

export const main = async () => {
   // Desktop tests
   await testURL("", [
      async page => {
         await checkHeaderAlignment(page)
         await waitFor(page, "h3.title")
         await checkWidthApprox(page, ".grid-container .flex-left-align", 800)
      }
   ])

   // Mobile tests
   await testMobile("", [
      async page => await checkHeaderAlignment(page)
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
