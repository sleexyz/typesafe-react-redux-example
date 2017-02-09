// @flow
const path = require('path');
const webpack = require('webpack');
const {
  sourcePaths,
  webpackBaseConfig,
  webpackLoaderRules,
} = require('../build_utils');

module.exports = {
  entry: [
    `mocha-loader!${path.resolve(sourcePaths.frontend, 'spec.js')}`,
    'webpack-hot-middleware/client',
  ],
  externals: undefined,
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
  plugins: [
    new webpack.HotModuleReplacementPlugin(),
    new webpack.NoEmitOnErrorsPlugin(),
  ],
  resolve: webpackBaseConfig.resolve,
  stats: 'errors-only',
  target: 'web',
};
