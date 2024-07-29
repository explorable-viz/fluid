/*
const http = require('http');
const process = require('node:process');
require('http-shutdown').extend();

const server = http.createServer((req, res) => {
  res.writeHead(200, { 'Content-Type': 'text/plain' });
  res.end('Hello World\n');
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
*/
function closeConnections() {
  console.log('Closing connections');
  connections.forEach((connection) => {
    connection.end('Server is restarting\n');
    connection.destroy();
    process.exit(0);
  });
}

process.on('SIGTERM', () => {
  console.log('Received SIGTERM');
  server.close(() => {
    console.log('Server closed ***SIGTERM***');
    closeConnections();
    process.exitCode = 0; //sets exit code whenever the process ends
    //process.exit(0); //ends process with exit code 0
  });
});

process.on('SIGINT', () => {
  console.log('Received SIGINT ###SIGINT###');
  server.close(() => {
    console.log('Server closed');
    closeConnections();
    process.exitCode = 0;
    //process.exit(0);
  });
});

process.on('uncaughtException', err => {
  console.log(`Uncaught Exception: ${err.message}`)
  process.exit(1)
});    

function moo(){
  server.close();
}

module.exports = {moo};
  /*
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
  */