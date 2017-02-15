// @flow
const path = require('path');
const nodeExternals = require('webpack-node-externals');
const {
  sourcePaths,
  webpackBaseConfig,
  webpackLoaderRules,
} = require('../build_utils');

module.exports = {
  devtool: 'cheap-module-source-map',
  entry: [
    path.resolve(sourcePaths.frontend, 'spec.js'),
  ],
  externals: [
    ...webpackBaseConfig.externals,
    nodeExternals(),
  ],
  module: {
    rules: [
      webpackLoaderRules.js,
    ],
  },
  output: {
    filename: 'index.js',
    path: path.resolve(__dirname, 'src', 'gen'),
  },
  plugins: undefined,
  resolve: webpackBaseConfig.resolve,
  stats: 'errors-only',
  target: 'node',
};
