path = require 'path'
nodeExternals = require 'webpack-node-externals'

module.exports =
  baseConfig:
    resolve:
      extensions: ['.js', '.json', '.coffee']
      modules: [
        'node_modules',
        path.resolve __dirname, 'src'
      ]
    externals: [nodeExternals()]

  loaderRules:
    coffee:
      exclude: /(node_modules|bower_components)/
      test: /\.coffee$/
      use: [
        {loader: 'babel-loader'}
        {loader: 'coffee-loader'}
      ]
    js:
      exclude: /(node_modules|bower_components)/
      test: /\.js$/
      use: [
        {loader: 'babel-loader'}
      ]

  sourcePaths:
    '0_code': path.resolve __dirname, '0_code', 'src'
