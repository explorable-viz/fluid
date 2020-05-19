module.exports = config => {
   config.set({
      autoWatch: true,
      browsers: ["Chrome"],
      files: [
         "dist/app.js",
         {
            pattern: "./fluid/**/*.fld",
            watched: true,
            included: false,
            served: true,
            nocache: false
         }
      ],
      reporters: ["spec"],
      singleRun: false
   })
}
