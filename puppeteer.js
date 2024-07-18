
import('./output-es/Test.Puppeteer/index.js').then(({ main }) => {
    main();
  }).catch(err => {
    console.error("Failed to load PureScript output:", err);
  });