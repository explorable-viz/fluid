var webpackConfig = require('./webpack.config');

module.exports = function (config) {
   config.set({
      basePath: '',
      frameworks: ['mocha', 'chai'],
      files: [
         './test/**/*.ts',
         {
            pattern: './example/*.lcalc',
            watched: true,
            included: false,
            served: true,
            nocache: false
         }
      ],
      exclude: [
         './test/Helpers.ts'
      ],
      preprocessors: {
         './bundle.js': ['webpack'],
         'test/**/*.ts': ['webpack']
      },
      webpack: {
         module: webpackConfig.module,
         resolve: webpackConfig.resolve,
         plugins: webpackConfig.plugins
      },
      webpackMiddleware: {
         // webpack-dev-middleware configuration
         // i. e.
         stats: 'errors-only'
      },
      mime: {
         'text/x-typescript': ['ts', 'tsx']
      },
      proxies: {
         '/example/': '/base/example/'
      },
      reporters: ['mocha', 'junit'],
      port: 8081,
      captureTimeout: 10000,
      colors: true,
      logLevel: config.LOG_ERROR,
      autoWatch: true,
      singleRun: false,
      concurrency: Infinity,
      browsers: ['ChromeHeadless'],
      customLaunchers: {
         'Chrome_with_debugging': {
            base: 'Chrome',
            flags: ['--remote-debugging-port=9222']
         }
      },
      junitReporter: {
         outputDir: 'node_modules/junit-test-results',
         outputFile: undefined,
         suite: '',
         useBrowserName: true,
         nameFormatter: undefined,
         classNameFormatter: undefined,
         properties: {}
      },
   })
}
 