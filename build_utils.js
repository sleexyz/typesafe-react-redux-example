// @flow
const path = require('path');

module.exports = {

  /*
     Meta
  */

  portMap: {
    dist_app: 8080,
    dist_test_web: 8081,
  },

  sourcePaths: {
    frontend: path.resolve(__dirname, 'frontend', 'src'),
  },

  /*
     Webpack
  */

  webpackBaseConfig: {
    resolve: {
      extensions: ['.js', '.json', '.coffee'],
      modules: [
        path.resolve(__dirname, 'frontend', 'src'),
        'node_modules',
      ],
    },
    externals: [
      {
        'react/addons': 'true',
        'react/lib/ExecutionEnvironment': 'true',
        'react/lib/ReactContext': 'true',
      },
    ],
  },

  webpackLoaderRules: {
    js: {
      exclude: /(node_modules|bower_components)/,
      test: /\.js$/,
      use: [
        { loader: 'babel-loader' },
      ],
    },
  },

};
