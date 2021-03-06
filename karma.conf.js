module.exports = function (config) {
   config.set({
      autoWatch: true,
      basePath: "",
      browsers: ["ChromeHeadless"],
      client: {
         mocha: {
           timeout : 20000
         }
      },
      colors: true,
      files: [
         "./dist/test/app.js",
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
