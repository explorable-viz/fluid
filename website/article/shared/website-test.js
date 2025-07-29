#!/usr/bin/env node
const express = require('express');
const serve = require('express-static');
require('http-shutdown').extend();

const app = express();

const root = process.cwd() + '/dist/' + process.argv[2];
app.use(serve(root));

const server = app.listen(8080, function() {
  console.log("Serving content from " + root);
}).withShutdown();

(async () => {
  try {
    if (process.argv.length == 4) {
      module = process.cwd() + process.argv[3];
    } else {
      module = root + '/test.mjs';
    }
    console.log('Loading Puppeteer test module:', module);
    import(module).then(({ main }) => {
      main().then(serverDown);
    }).catch(err => {
      console.error("Failed to load PureScript output:", err);
    });
  } catch (error) {
    console.error('Error:', error);
  }
})();

function serverDown() {
  console.log('Shutting down server')
  server.shutdown(function(err) {
    if (err) {
      return console.log('shutdown failed', err.message);
    }
    console.log('Everything is cleanly shut down.');
  });
}
