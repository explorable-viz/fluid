var HtmlWebpackPlugin = require('html-webpack-plugin');
var path = require('path');

module.exports = {
   mode: "development",
   entry: {
      main: "./src/Lib.ts"
   },
   output: {
      filename: "bundle.js",
      library: "Fluid", // used as name of variable or field of "exports" object
//      libraryExport: "./src/Lib.ts", // not sure what the notion of "module" is here
      libraryTarget: "commonjs", // previously "var"; "commonjs2" leaves fluid_1 unresolved on client
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
      new HtmlWebpackPlugin(),
      // Plugin to show any webpack warnings and prevent tests from running.
      // Based on https://gist.github.com/Stuk/6b574049435df532e905.
      function () {
         var errors = []
         this.plugin("done", function (stats) {
             if (stats.compilation.errors.length ||
                 stats.compilation.warnings.length) {
                 // Log each of the warnings
                 stats.compilation.errors.forEach(function (error) {
                     errors.push(error.message || error)
                 })

                 // Pretend no assets were generated. This prevents the tests
                 // from running making it clear that there were warnings.
                 stats.stats = [{
                     toJson: function () {
                         return this;
                     },
                     assets: []
                 }];
             }
         });
      }
   ],
   devtool: "inline-source-map"
}
