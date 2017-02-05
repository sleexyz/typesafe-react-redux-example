const path = require('path');
const nodeExternals = require('webpack-node-externals');

module.exports = {

  /*
     Meta-Framework
  */

  sourcePaths: {
    '0_code': path.resolve(__dirname, '0_code', 'src'),
  },

  /*
     Webpack
  */

  webpackBaseConfig: {
    resolve: {
      extensions: ['.js', '.json', '.coffee'],
      modules: [
        'node_modules',
        path.resolve(__dirname, 'src'),
      ],
    },
    externals: [nodeExternals()],
  },
  webpackLoaderRules: {
    js: {
      exclude: /(node_modules|bower_components)/,
      test: /\.js$/,
      use: [
        {loader: 'babel-loader'},
      ],
    }
  },
};
