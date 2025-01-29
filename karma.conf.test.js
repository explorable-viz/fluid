module.exports = function (config) {
   config.set({
      autoWatch: true,
      basePath: "",
      browsers: ["ChromeHeadlessNoSandbox"],
      browserDisconnectTimeout: 600000,
      browserNoActivityTimeout: 2400000,
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
            timeout: 600000
         }
      },
      colors: true,
      failOnEmptyTestSuite: true,
      files: [
         "./dist/test/fluid.js",
         {
            pattern: "./fluid/**/*.fld",
            watched: true,
            included: false,
            served: true,
            nocache: false
         },
         {
            pattern: "./test/**/*.fld",
            watched: true,
            included: false,
            served: true,
            nocache: false,
         }
      ],
      frameworks: ["mocha"],
      proxies: {
         "/fluid/": "/base/fluid/",
         "/test/fluid/": "/base/test/fluid/"
      },
      reporters: ["mocha"],
      singleRun: true
   })
}
