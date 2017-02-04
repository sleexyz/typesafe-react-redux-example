path = require 'path'
webpack = require 'webpack'
{baseConfig, loaderRules, sourcePaths} = require '../build_utils'


module.exports =
  entry: [
    path.resolve sourcePaths['0_code'], 'index.coffee'
    'webpack-hot-middleware/client'
  ]
  externals: undefined
  module:
    rules: [
      loaderRules.coffee
      loaderRules.js
    ]
  output:
    filename: 'index.js'
    path: path.resolve __dirname, 'src', '0_code'
  plugins: [
    new webpack.HotModuleReplacementPlugin()
    new webpack.NoEmitOnErrorsPlugin()
  ]
  resolve: baseConfig.resolve
  stats: 'errors-only'
  target: 'web'
