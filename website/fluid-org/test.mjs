import { testURL, waitFor, checkTextContent } from "@explorable-viz/fluid/script/webtest-lib.mjs"

export const main = async () => {
   await testURL("", [
      async page => {
         await waitFor(page, ".grid-container")
         await waitFor(page, "h3.title")
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
