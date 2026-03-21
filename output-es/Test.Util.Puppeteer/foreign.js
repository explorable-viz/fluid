import puppeteer from "puppeteer";

export function _launch(options) {
  return function() {
    return puppeteer.launch(options);
  };
}
