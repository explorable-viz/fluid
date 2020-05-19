module.exports = function (config) {
   config.set({
      basePath: "",
      client: {
         mocha: {
           timeout: 60000 // default 2000; runs much slower in CircleCI environment
         }
      },
      files: [
         "./dist/app.js",
         {
            pattern: "./fluid/**/*.fld",
            watched: true,
            included: false,
            served: true,
            nocache: false
         }
      ],
      proxies: {
         "/fluid/": "/base/fluid/"
      },
      reporters: ["mocha"],
      port: 1234,
      captureTimeout: 30000,
      browserDisconnectTimeout : 30000,
      browserNoActivityTimeout : 60000,
      colors: true,
//      logLevel: config.LOG_ERROR,
      autoWatch: true,
      singleRun: false,
      concurrency: Infinity,
//      browsers: ["ChromeHeadless"],
      customLaunchers: {
         "Chrome_with_debugging": {
            base: "Chrome",
            flags: ["--remote-debugging-port=9222"]
         }
      }
   })
}
