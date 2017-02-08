#!/usr/bin/env node
const webpack = require('webpack');
const config = require('./webpack');

webpack(config, (err, stats) => {
  if (err) {
    console.error(err);
  }
});
