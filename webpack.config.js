var HtmlWebpackPlugin = require('html-webpack-plugin');
var path = require('path');

module.exports = {
   mode: "development",
   entry: {
      main: "./src/app/App.ts", 
      lib: "./src/Lib.ts"
   },
   // This took a lot of painful trial-and-error to figure out. Some notes:
   // - "commonjs2" causes module not to resolve on the client (silently, of course)
   // - specifying "libraryExport" (silently) overrides the exports of the module; avoid
   // - specifying "library" adds an extra level of indirection (fluid_1.Fluid) which breaks client
   output: {
      filename: "[name].bundle.js",
      library: "", // avoids extra indirection; can I drop entirely?
      libraryTarget: "commonjs",
      path: path.resolve(__dirname, "dist")
   },
   resolve: {
      // Add '.ts' as a resolvable extension.
      extensions: [".webpack.js", ".web.js", ".ts", ".js"]
   },
   module: {
      rules: [
         // all files with a '.ts' or '.tsx' extension will be handled by 'ts-loader'
         {
             test: /\.ts$/,
             loader: "ts-loader"
         },
         // css files; import css files in .ts using 'import' or 'require'
         {
             test: /\.css$/,
             loaders: ["style-loader", "css-loader"]
         }
      ]
   },
   devServer: {
      port: 8000
   },
   plugins: [
      // cobbled together from https://github.com/webpack-contrib/karma-webpack/issues/66
      function()
      {
          this.plugin("done", function(stats)
          {
               if (stats && stats.hasErrors()) {
                  stats.toJson().errors.forEach(err => {
                    console.error(err)
                  })
                  throw new Error()
               }
          });
      }
  ],
  devtool: "inline-source-map"
}
