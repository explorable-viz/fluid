const http = require('http');
const process = require('node:process');
require('http-shutdown').extend();

const server = http.createServer((req, res) => {
  res.writeHead(200, { 'Content-Type': 'text/plain' });
  res.end('Server created\n');
}).withShutdown();

server.listen(8080, () => {
  console.log('Server started');
});

let connections = [];

server.on('connection', (connection) => {
  connections.push(connection);
  connection.on('close', () => {
    connections = connections.filter((curr) => {
      return curr !== connection;
    });
  });
});
/////////
function serverDown() {
  server.close();
  server.shutdown(function(err) {
    if (err) {
        return console.log('shutdown failed', err.message);
    }
    console.log('Everything is cleanly shutdown.');
  });
};
module.exports = {serverDown};
/////////

import('./output-es/Test.Puppeteer/index.js').then(({ main }) => {
    main();
    //serverDown();
  }).catch(err => {
    console.error("Failed to load PureScript output:", err);
  });

///////////





  
