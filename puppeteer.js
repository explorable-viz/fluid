const express = require('express');
const serve = require('express-static');
require('http-shutdown').extend();

const app = express();

const root = __dirname + '/dist/' + process.argv[3];
app.use(serve(root));

const server = app.listen(8080, function() {
  console.log("Serving content from " + root);
}).withShutdown();

(async () => {
  try {
    if (process.argv.length == 5 && process.argv[4] == "-l") {
      module = "../../../../../".concat(process.argv[2])
    } else {
      module = process.argv[2]
    }
    console.log('Loading Puppeteer test module:', module);
    import( module ).then(({ main }) => {
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
