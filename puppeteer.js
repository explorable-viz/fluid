const express = require('express');
const serve = require('express-static');
require('http-shutdown').extend();

const app = express();

app.use(serve(__dirname + '/dist/' + process.argv[3]));

const server = app.listen(8080, function() {
  console.log("Server running");
}).withShutdown();

(async () => {
  try {
    const module = process.argv[2]
    console.log('Loading module:', module);
    import('./output-es/' + module + '/index.js').then(({ main }) => {
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
