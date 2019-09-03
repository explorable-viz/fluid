var webpackConfig = require('./webpack.config')[0] // ouch - pick out first configuration!

module.exports = function (config) {
   config.set({
      basePath: '',
      client: {
         mocha: {
           timeout: 60000 // default 2000; runs much slower in CircleCI environment
         }
      },
      frameworks: ['mocha', 'chai'],
      files: [
         './test/**/Test.ts',
         {
            pattern: './lcalc/**/*.lcalc',
            watched: true,
            included: false,
            served: true,
            nocache: false
         }

      ],
      exclude: [
         // otherwise these will also appear as entry points
         './test/util/*',
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
         // i.e.
         stats: 'errors-only'
      },
      mime: {
         'text/x-typescript': ['ts', 'tsx']
      },
      proxies: {
         '/lcalc/': '/base/lcalc/'
      },
      reporters: ['mocha'],
      port: 8081,
      captureTimeout: 30000,
      browserDisconnectTimeout : 30000,
      browserNoActivityTimeout : 60000,
      colors: true,
      logLevel: config.LOG_ERROR,
      autoWatch: true,
      singleRun: true,
      concurrency: Infinity,
      browsers: ['ChromeHeadless'],
      customLaunchers: {
         'Chrome_with_debugging': {
            base: 'Chrome',
            flags: ['--remote-debugging-port=9222']
         }
      },
   })
}
