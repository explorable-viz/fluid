#!/usr/bin/env node
const fs = require('node:fs');

(async () => {
   tester = await import('./output-es/Test.Fluid.Fluid/index.js');   
   tester.main()
})();