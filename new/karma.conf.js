module.exports = function (config) {
   config.set({
      autoWatch: true,
      basePath: "",
      browsers: ["ChromeHeadless"],
      client: {
         mocha: {
           timeout : 6000 // 6 seconds - upped from 2 seconds
         }
      },
      colors: true,
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
      frameworks: ["mocha"],
      proxies: {
         "/fluid/": "/base/fluid/"
      },
      reporters: ["mocha"],
      singleRun: true
   })
}
