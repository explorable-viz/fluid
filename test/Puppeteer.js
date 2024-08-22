import puppeteer from "puppeteer";

export function _launchFirefox() {
    return puppeteer.launch({ browser: "firefox" })
}

export function checkLineChartPlotPoints(page) {
   return async function() {
      // Getting an array of all elements with class 'item'
      const elements = await page.$$('.item'); // selector that specifies the line chart points

      // Looping through and interacting with each element
      for (const element of elements) {
         await element.click(); // replace with whatever test code
      }
   }
}
