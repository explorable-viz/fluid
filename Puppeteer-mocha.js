
const puppeteer = require('puppeteer');
const mocha = require('mocha');
const { describe, it, before, after } = mocha;
const express = require('express');
const serve = require('express-static');
require('http-shutdown').extend();
const app = express();
const assert =require('assert');
const { run } = require('./output-es/Test.Puppeteer/index.js')


describe('Got to web page', () => {
  let browser;
  let page;
  let server;

  before(async () => {
    //Start the server
    app.use(serve(__dirname + '/dist/app'));
    server = app.listen(8080, function(){
      console.log("Server running");
    }).withShutdown();

    //Launch Puppeteer browser
    browser = await puppeteer.launch();
    page = await browser.newPage();
  });

  after(async () => {
    console.log ("Closing browser")
    await browser.close();
    serverDown(server)
  });

  /*
  it('should display the correct title', async () => {
    await page.goto('http://127.0.0.1:8080');
    const content = await page.content();
    console.log(content);
    const title = await page.title();
    assert.equal(title, 'Fluid: Data-Linked Visualisations');
  });
  */

  it('execute Purescript tests', async function(){
    await run();
  });
});

function serverDown(server)
{
  console.log('Shutting down server')
  server.shutdown(function(err) {
    if (err) {
      return console.log('Shutdown failed', err.message);
    }
    console.log('Everything is cleanly shutdown.');
  });
}