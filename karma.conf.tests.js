module.exports = function (config) {
   config.set({
      autoWatch: true,
      basePath: "",
      browsers: ["ChromeHeadlessNoSandbox"],
      browserDisconnectTimeout: 10000,
      customLaunchers: {
         ChromeHeadlessNoSandbox: {
           base: 'ChromeHeadless',
            flags: [
               '--disable-gpu',
               '--no-sandbox'
            ]
         }
      },
      client: {
         mocha: {
            timeout: 30000
         }
      },
      colors: true,
      failOnEmptyTestSuite: false,
      files: [
         "./dist/tests/app.js",
         {
            pattern: "./fluid/**/*.fld",
            watched: true,
            included: false,
            served: true,
            nocache: false
         }
      ],
      frameworks: ["mocha"],
      proxies: {
         "/fluid/": "/base/fluid/"
      },
      reporters: ["mocha"],
      singleRun: true
   })
}
