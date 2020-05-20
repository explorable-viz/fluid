const CnamePlugin = require("cname-webpack-plugin")
const CopyPlugin = require("copy-webpack-plugin")
const path = require("path")

// Specify two configs; first one (app) picked up automatically by webpack-dev-server. Second is lib.
// TODO: use inheritance to factor out common elements.
module.exports = [{
   entry: {
      main: "./src/app/Demo.ts"
   },
   output: {
      filename: "[name].bundle.js",
      publicPath: "/"
   },
   resolve: {
      // Add '.ts' as a resolvable extension.
      extensions: [".webpack.js", ".web.js", ".ts", ".js"]
   },
   optimization: {
      minimize: false // for now (we rely heavily on reflected class names)
   },
   module: {
      rules: [
         {
            test: /\.ts$/,
            loader: "ts-loader"
         },
         {
            test: /\.css$/,
            loaders: ["style-loader", "css-loader"]
         },
         {
            test: /\.woff$/,
            use: {
               loader: "url-loader",
               options: {
                 limit: 100000,
               },
            }
         }
      ]
   },
   plugins: [
      // cobbled together from https://github.com/webpack-contrib/karma-webpack/issues/66
      function ()
      {
          this.plugin("done", function (stats)
          {
               if (stats && stats.hasErrors()) {
                  stats.toJson().errors.forEach(err => {
                    console.error(err)
                  })
                  throw new Error()
               }
          });
      },
      new CnamePlugin(
         { domain: "f.luid.org" }
      ),
      new CopyPlugin([
         { from: "fluid/**/*" }
      ])
   ],
   devtool: "inline-source-map"
},{
   mode: "development",
   entry: {
      lib: "./src/Lib.ts"
   },
   // This took a *lot* of painful trial-and-error to figure out. Some notes:
   // - "commonjs2" causes module not to resolve on the client (silently, of course)
   // - "libraryExport" (silently) overrides the exports of the module; avoid
   // - non-empty "library" adds an extra level of indirection (fluid_1.Fluid) which breaks client
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
         {
             test: /\.ts$/,
             loader: "ts-loader"
         },
         {
             test: /\.css$/,
             loaders: ["style-loader", "css-loader"]
         },
         {
            test: /\.woff$/,
            use: {
               loader: "url-loader",
               options: {
                 limit: 100000,
               },
            }
         }
      ]
   },
   plugins: [
      new CopyPlugin([
         { from: "fluid/**/*" }
      ])
   ],
   devtool: "inline-source-map"
}]
