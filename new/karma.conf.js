module.exports = function (config) {
   config.set({
      basePath: "",
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
      frameworks: ["mocha"],
      reporters: ["mocha"],
   })
}
