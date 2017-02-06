#!/usr/bin/env node
const webpack = require('webpack');
const webpackDev = require('webpack-dev-middleware');
const webpackHot = require('webpack-hot-middleware');
const express = require('express');
const path = require('path');
const config = require('./webpack');
const {portMap} = require('../build_utils.js');


const port = portMap['1_app'];

const compiler = webpack(config);

const app = express()
  .use(webpackDev(compiler, {
    publicPath: config.output.publicPath,
    stats: 'errors-only',
  }))
  .use(webpackHot(compiler))
  .use('*', (req, res) => {
    res.sendFile(path.resolve(__dirname, 'src/index.html'));
  });

app.listen(port, (err) => {
  if (err) {
    console.error(err);
  }
  console.log(`listening at http://localhost:${port}`);
});
