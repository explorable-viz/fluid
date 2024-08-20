const process = require('process');

import('./output-es/Fluid/index.js').then(({ main }) => {
    args = process.argv.slice(2) //ignore first 2 elements (node and file path)
    console.log(args)
    //main();
  })