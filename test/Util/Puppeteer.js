import puppeteer from "puppeteer";

export function _launchFirefox() {
    return puppeteer.launch({ browser: "firefox" })
}
