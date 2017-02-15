#!/usr/bin/env node
const webpack = require('webpack');
const webpackDev = require('webpack-dev-middleware');
const webpackHot = require('webpack-hot-middleware');
const express = require('express');
const serveStatic = require('serve-static');
const path = require('path');
const config = require('./webpack');
const { portMap } = require('../build_utils.js');

const port = portMap.dist_test_web;

const compiler = webpack(config);

const app = express();
app.use(webpackDev(compiler, {
  publicPath: config.output.publicPath,
  stats: 'errors-only',
}));
app.use(webpackHot(compiler));
app.use(serveStatic(path.resolve(__dirname, 'src')));
app.use('*', (req, res) => {
  res.sendFile(path.resolve(__dirname, 'src/index.html'));
});
app.listen(port, (err) => {
  if (err) {
    // eslint-disable-next-line no-console
    console.error(err);
  }
  // eslint-disable-next-line no-console
  console.log(`listening at http://localhost:${port}`);
});
