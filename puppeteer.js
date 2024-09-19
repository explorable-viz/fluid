const express = require('express');
const serve = require('express-static');
require('http-shutdown').extend();

const app = express();

app.use(serve(__dirname + '/dist/app'));

const server = app.listen(8080, function() {
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

function serverDown() {
  console.log('Shutting down server')
  server.shutdown(function(err) {
    if (err) {
      return console.log('shutdown failed', err.message);
    }
    console.log('Everything is cleanly shutdown.');
  });
}
