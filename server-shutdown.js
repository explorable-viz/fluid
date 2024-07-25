const process = require('node:process');
const http = require('http');

process.on('SIGTERM', () => {
  console.log('kill');
});