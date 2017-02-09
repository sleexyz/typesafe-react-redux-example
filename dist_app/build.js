#!/usr/bin/env node
const webpack = require('webpack');
const config = require('./webpack');

webpack(config, (err) => {
  if (err) {
    // eslint-disable-next-line no-console
    console.error(err);
  }
});
