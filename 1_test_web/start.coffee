webpack = require 'webpack'
webpackDev = require 'webpack-dev-middleware'
webpackHot = require 'webpack-hot-middleware'
express = require 'express'
path = require 'path'
config = require './webpack'


compiler = webpack config

app = express()
  .use webpackDev compiler,
    publicPath: config.output.publicPath,
    stats: 'errors-only'

  .use webpackHot compiler,
    log: console.log

  .use '*', (req, res) ->
    res.sendFile path.resolve __dirname, 'test_server/index.html'

app.listen 8080, (err) ->
  console.error err if err
  console.log 'listening at http://localhost:8080'
