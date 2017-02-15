// @flow
const path = require('path');
const webpack = require('webpack');
const {
  sourcePaths,
  webpackBaseConfig,
  webpackLoaderRules,
} = require('../build_utils');

module.exports = {
  devtool: 'cheap-module-source-map',
  entry: [
    `mocha-loader!${path.resolve(sourcePaths.frontend, 'spec.js')}`,
    'webpack-hot-middleware/client',
  ],
  externals: webpackBaseConfig.externals,
  module: {
    rules: [
      webpackLoaderRules.js,
    ],
  },
  output: {
    filename: 'index.js',
    path: path.resolve(__dirname, 'src'),
    publicPath: '/gen/',
  },
  node: {
    fs: 'empty',
    module: 'empty',
  },
  plugins: [
    new webpack.HotModuleReplacementPlugin(),
    new webpack.NoEmitOnErrorsPlugin(),
  ],
  resolve: webpackBaseConfig.resolve,
  stats: 'errors-only',
  target: 'web',
};
