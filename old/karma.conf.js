const webpackConfig = require("./webpack.config")[0] // ouch - pick out first configuration!

module.exports = function (config) {
   config.set({
      basePath: "",
      client: {
         mocha: {
           timeout: 60000 // default 2000; runs much slower in CircleCI environment
         }
      },
      frameworks: ["mocha"],
      files: [
         "./test/**/*.ts",
         {
            pattern: "./fluid/**/*.fld",
            watched: true,
            included: false,
            served: true,
            nocache: false
         }
      ],
      exclude: [
         // otherwise these will also appear as entry points
         "./test/util/*",
      ],
      preprocessors: {
         "./src/**/*.ts": ["webpack"],
         "test/**/*.ts": ["webpack"]
      },
      webpack: {
         module: webpackConfig.module,
         resolve: webpackConfig.resolve,
         plugins: webpackConfig.plugins
      },
      webpackMiddleware: {
         stats: "errors-only"
      },
      mime: {
         "text/x-typescript": ["ts", "tsx"]
      },
      proxies: {
         "/fluid/": "/base/fluid/"
      },
      reporters: ["mocha"],
      port: 8081,
      captureTimeout: 30000,
      browserDisconnectTimeout : 30000,
      browserNoActivityTimeout : 60000,
      colors: true,
      logLevel: config.LOG_ERROR,
      autoWatch: true,
      singleRun: true,
      concurrency: Infinity,
      browsers: ["ChromeHeadless"],
      customLaunchers: {
         "Chrome_with_debugging": {
            base: "Chrome",
            flags: ["--remote-debugging-port=9222"]
         }
      },
   })
}
