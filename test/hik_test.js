

/*
const puppeteer = require('puppeteer');


  exports.screenshotImpl = async function (path, page) {
    await page.screenshot({ path });
    return;
  };
  */

  const puppeteer = require('puppeteer');

  (console.log("ll"));


  (async () => {
    const browser = await puppeteer.launch();
    const page = await browser.newPage();
    await page.goto('http://127.0.0.1:8080/');
  });