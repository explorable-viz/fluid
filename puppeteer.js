
const express = require('express');
const serve = require('express-static');
require('http-shutdown').extend();

const app = express();

app.use(serve(__dirname + '/dist/app'));

const server = app.listen(8080, function(){
  console.log("Server running");
}).withShutdown();

(async () => {
  try {
    import('./output-es/Test.Puppeteer/index.js').then(({ main }) => {
      main().then(serverDown);
    }).catch(err => {
      console.error("Failed to load PureScript output:", err);
    });
  } catch (error) {
    console.error('Error:', error);
  }
})();

function serverDown()
{
  console.log('Shutting down server')
  server.shutdown(function(err) {
    if (err) {
      return console.log('shutdown failed', err.message);
    }
    console.log('Everything is cleanly shutdown.');
  });

}


///////////////////////

/*
const {setTimeout} = require('timers/promises');
const http = require('http');
const express = require('express');
const serve = require('express-static');
const puppeteer = require('puppeteer');
const fs = require('fs')
const path = require('path');
require('http-shutdown').extend();

const app = express();

app.use(serve(__dirname + '/dist/app'));

const server = app.listen(8080, function(){
  console.log("Server running");
}).withShutdown();    

const userDataDir = path.resolve('./user-data');

if (!fs.existsSync(userDataDir)) {
    fs.mkdirSync(userDataDir, { recursive: true });
}

(async () => {
    const browser = await puppeteer.launch();
    try {
        console.log('Launching browser')
        
        const page = await browser.newPage();
        page.on('console', msg => console.log('PAGE LOG:', msg.text()))
        await page.goto('http://127.0.0.1:8080', {waituntil: 'domcontentloaded'});
        
        const content = await page.content();
        console.log(content);

        
        const selector = `svg#fig-4-output`;
        console.log(`Waiting for ${selector}`);
        await page.waitForSelector(selector, { timeout: 120000 });
        console.log(`Found ${selector}`); 

        await checkForFigure(page, "fig-4-output");
        await checkForFigure(page, "fig-1-bar-chart");
        await checkForFigure(page, "fig-1-line-chart");
        await checkForFigure(page, "fig-conv-2-output");
        
    } catch (error) {
        console.error('Error:', error);
    }
    await browser.close();
    console.log("Browser closed");
    console.log('Shutting down server')
    await server.shutdown(function(err) {
      if (err) {
        return console.log('shutdown failed', err.message);
      }
      console.log('Everything is cleanly shutdown.');
      
    });
  })();

  async function checkForFigure(page, id) {
    const selector = `svg#${id}`;
    console.log(`Waiting for ${selector}`);
    await page.waitForSelector(selector, { timeout: 120000 });
    console.log(`Found ${selector}`); 
  }
*/
