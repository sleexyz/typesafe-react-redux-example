const path = require('path');
const webpack = require('webpack');
const {
  sourcePaths,
  webpackBaseConfig,
  webpackLoaderRules,
} = require('../build_utils');


module.exports = {
  entry: [
    path.resolve(sourcePaths['0_code'], 'index.js'),
    'webpack-hot-middleware/client',
  ],
  externals: undefined,
  module: {
    rules: [
      webpackLoaderRules.js,
    ],
  },
  output: {
    filename: path.join('gen', 'index.js'),
    path: path.resolve(__dirname, 'src'),
  },
  plugins: [
    new webpack.HotModuleReplacementPlugin(),
    new webpack.NoEmitOnErrorsPlugin(),
  ],
  resolve: webpackBaseConfig.resolve,
  stats: 'errors-only',
  target: 'web',
};
