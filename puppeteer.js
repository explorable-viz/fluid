import('./output-es/Test.Puppeteer/index.js').then(({ main }) => {
    main();
  }).catch(err => {
    console.error("Failed to load PureScript output:", err);
  });

import('./server.js').then(({ serverDown }) => {
  serverDown();
}).catch(err => {
  console.error("Failed to load serverDown:", err);
});