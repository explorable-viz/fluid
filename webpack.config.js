var HtmlWebpackPlugin = require('html-webpack-plugin');
var path = require('path');

module.exports = {
   entry: ["./src/app/App.ts"],
   output: {
      filename: "bundle.js",
      library: "LambdaCalc",
      libraryTarget: "var"
   },
   resolve: {
      // Add '.ts' as a resolvable extension.
      extensions: ["", ".webpack.js", ".web.js", ".ts", ".js"]
   },
   module: {
      loaders: [
         // all files with a '.ts' or '.tsx' extension will be handled by 'ts-loader'
         {
             test: /\.ts$/,
             loader: "ts-loader"
         },
         // css files; import css files in .ts using 'import' or 'require'
         {
             test: /\.css$/,
             loaders: ["style-loader", "css-loader"]
         },
         // Embed all .lcalc files as URLs inside the final JS bundle
         // https://github.com/webpack/webpack/issues/7353
         {
            test: /\.lcalc$/,
            loader: "url-loader",
            options: {
              limit: Infinity
            }
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
