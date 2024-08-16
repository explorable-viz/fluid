import puppeteer from "puppeteer";

export function _launchFirefox(options) {
  return function() {
    return puppeteer.launch({ browser: "firefox" });
  };
}