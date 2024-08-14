const { argv } = require('node:process');

// print process.argv; testing
argv.forEach((val, index) => {
    console.log(`${index}: ${val}`);
  });

// 