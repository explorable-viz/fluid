/*
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
*/

///////////////////////


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

const waitTillHTMLRendered = async (page, timeout = 30000) => {
    const checkDurationMsecs = 1000;
    const maxChecks = timeout / checkDurationMsecs;
    let lastHTMLSize = 0;
    let checkCounts = 1;
    let countStableSizeIterations = 0;
    const minStableSizeIterations = 3;
  
    while(checkCounts++ <= maxChecks){
      let html = await page.content();
      let currentHTMLSize = html.length; 
  
      let bodyHTMLSize = await page.evaluate(() => document.body.innerHTML.length);
  
      console.log('last: ', lastHTMLSize, ' <> curr: ', currentHTMLSize, " body html size: ", bodyHTMLSize);
  
      if(lastHTMLSize != 0 && currentHTMLSize == lastHTMLSize) 
        countStableSizeIterations++;
      else 
        countStableSizeIterations = 0; //reset the counter
  
      if(countStableSizeIterations >= minStableSizeIterations) {
        console.log("Page rendered fully..");
        break;
      }
  
      lastHTMLSize = currentHTMLSize;
      await setTimeout(checkDurationMsecs);
    }  
  };



(async () => {
    const browser = await puppeteer.launch();
    try {
        console.log('Launching browser')
        
        const page = await browser.newPage();
        page.on('console', msg => console.log('PAGE LOG:', msg.text()))
        await page.goto('http://127.0.0.1:8080', {waituntil: 'domcontentloaded'});
        await waitTillHTMLRendered(page)
        
        const content = await page.content();
        console.log(content);

        //await checkForFigure(page, "fig-4");
        const selector = `svg#fig-4-output`;
        console.log(`Waiting for ${selector}`);
        await page.waitForSelector(selector, { timeout: 120000 });
        console.log(`Found ${selector}`); 

        //await checkForFigure(page, "fig-1");
        //await checkForFigure(page, "fig-conv-2");
        
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
    const selector = `div#${id}`;
    console.log(`Waiting for ${selector}`);
    await page.waitForSelector(selector, { timeout: 120000 });
    console.log(`Found ${selector}`); 
  }

