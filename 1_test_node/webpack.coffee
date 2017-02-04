path = require 'path'
{baseConfig, loaderRules, sourcePaths} = require '../build_utils'


module.exports =
  entry: [
    path.resolve sourcePaths['0_code'], 'spec.coffee'
  ]
  externals: baseConfig.externals
  module:
    rules: [
      loaderRules.coffee
      loaderRules.js
    ]
  output:
    filename: 'index.js'
    path: path.resolve __dirname, 'src', '0_code'
  plugins: undefined
  resolve: baseConfig.resolve
  stats: 'errors-only'
  target: 'node'
