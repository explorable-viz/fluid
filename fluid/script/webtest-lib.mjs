import puppeteer from "puppeteer"

const TIMEOUT = 60000
const LOGGING = true
const HEADLESS = process.env.HEADLESS !== "false"
const DESKTOP = { width: 1200, height: 800, deviceScaleFactor: 1.0 }
const MOBILE = { width: 390, height: 844, deviceScaleFactor: 2.0, isMobile: true }
const VIEWPORT = process.env.MOBILE ? MOBILE : DESKTOP

function log(msg) {
   if (LOGGING) console.log(msg)
}

function testOutcome(pass, msg) {
   const sym = pass ? "\x1b[32m ✔\x1b[0m" : "\x1b[31m ✖\x1b[0m"
   console.log(`${sym} ${msg}`)
   if (!pass) throw new Error("Test failed")
}

async function launchBrowser(browserName) {
   return puppeteer.launch({
      browser: browserName,
      headless: HEADLESS,
      defaultViewport: VIEWPORT,
   })
}

export async function waitFor(page, selector, { visible = true } = {}) {
   log(`Waiting for ${selector}${visible ? "" : " (any)"}`)
   try {
      await page.waitForSelector(selector, { timeout: TIMEOUT, visible })
      log("-> found")
      testOutcome(true, `${selector}: exists`)
   } catch (e) {
      testOutcome(false, `${selector}: ${e.message}`)
   }
}

export async function waitForHidden(page, selector) {
   log(`Waiting for ${selector} (hidden)`)
   await page.waitForSelector(selector, { timeout: TIMEOUT, visible: false })
   log("-> found")
}

export async function click(page, selector) {
   await page.click(selector)
   testOutcome(true, `${selector}: click`)
}

export async function dispatchMouseDown(page, selector) {
   await page.evaluate(sel => {
      document.querySelector(sel).dispatchEvent(new MouseEvent('mousedown', { bubbles: true }))
   }, selector)
   testOutcome(true, `${selector}: mousedown`)
}

export async function checkAttribute(page, selector, attr, expected) {
   const found = await page.$eval(selector, (el, a) => el.getAttribute(a), attr)
   const pass = found === expected
   const errorMsg = pass ? "" : ` (got "${found}")`
   testOutcome(pass, `${selector}: ${attr} == "${expected}"${errorMsg}`)
}

export async function checkAttributeContains(page, selector, attr, expected) {
   const found = await page.$eval(selector, (el, a) => el.getAttribute(a), attr)
   const pass = found.includes(expected)
   const errorMsg = pass ? "" : ` (got "${found}")`
   testOutcome(pass, `${selector}: ${attr} contains "${expected}"${errorMsg}`)
}

export async function checkTextContent(page, selector, expected) {
   await waitFor(page, selector)
   const text = await page.$eval(selector, el => el.textContent)
   const pass = text === expected
   testOutcome(pass, `${selector}: text == "${expected}"${pass ? "" : ` (got "${text}")`}`)
}

export async function checkComputedStyle(page, selector, property, expected) {
   await waitFor(page, selector)
   const value = await page.$eval(selector, (el, prop) => getComputedStyle(el)[prop], property)
   const pass = value === expected
   testOutcome(pass, `${selector}: ${property} == "${expected}"${pass ? "" : ` (got "${value}")`}`)
}

export async function checkCount(page, selector, expected) {
   const count = await page.$$eval(selector, els => els.length)
   const pass = count === expected
   testOutcome(pass, `${selector}: count == ${expected}${pass ? "" : ` (got ${count})`}`)
}

export async function checkCountAtLeast(page, selector, minimum) {
   const count = await page.$$eval(selector, els => els.length)
   const pass = count >= minimum
   testOutcome(pass, `${selector}: count >= ${minimum}${pass ? ` (got ${count})` : ` (got ${count})`}`)
}

export async function getBoundingBox(page, selector) {
   return page.$eval(selector, el => {
      const r = el.getBoundingClientRect()
      return { left: r.left, right: r.right, top: r.top, bottom: r.bottom, width: r.width, height: r.height }
   })
}

export async function checkAlignment(page, sel1, sel2, edge) {
   const box1 = await getBoundingBox(page, sel1)
   const box2 = await getBoundingBox(page, sel2)
   const val1 = Math.round(box1[edge])
   const val2 = Math.round(box2[edge])
   const pass = Math.abs(val1 - val2) <= 1
   testOutcome(pass, `${sel1} and ${sel2}: ${edge} aligned${pass ? "" : ` (${val1} vs ${val2})`}`)
}

export async function checkWidthApprox(page, selector, expected, tolerance = 5) {
   const box = await getBoundingBox(page, selector)
   const pass = Math.abs(box.width - expected) <= tolerance
   testOutcome(pass, `${selector}: width ≈ ${expected}${pass ? "" : ` (got ${Math.round(box.width)})`}`)
}

export async function clickToggle(page) {
   await waitFor(page, "#grid.data-pane-hidden")
   const toggle = "button[title='Show data pane']"
   await waitFor(page, toggle)
   await click(page, toggle)
   await waitFor(page, "#grid:not(.data-pane-hidden)")
}

async function browserTests(url, browserName, viewport, tests) {
   const label = viewport === MOBILE ? "mobile" : "desktop"
   log(`browserTests: ${browserName} (${label})`)
   const browser = await launchBrowser(browserName)
   const page = await browser.newPage()
   await page.setViewport(viewport)
   for (const test of tests) {
      await page.goto(url)
      await test(page)
   }
   await browser.close()
}

const BASE_URL = process.env.BASE_URL || "http://127.0.0.1:8080"

export async function testURL(suffix, tests) {
   const url = `${BASE_URL}/${suffix}`
   await browserTests(url, "chrome", VIEWPORT, tests)
   await browserTests(url, "firefox", VIEWPORT, tests)
}

export async function testMobile(suffix, tests) {
   const url = `${BASE_URL}/${suffix}`
   await browserTests(url, "chrome", MOBILE, tests)
   await browserTests(url, "firefox", MOBILE, tests)
}
