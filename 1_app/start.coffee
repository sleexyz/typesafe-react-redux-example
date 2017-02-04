webpack = require 'webpack'
webpackDev = require 'webpack-dev-middleware'
webpackHot = require 'webpack-hot-middleware'
express = require 'express'
config = require './webpack'
path = require 'path'


compiler = webpack config

app = express()
  .use webpackDev compiler,
    publicPath: config.output.publicPath,
    stats: 'errors-only'
  .use webpackHot compiler
  .use '*', (req, res) ->
    res.sendFile path.resolve __dirname, 'src/index.html'
app.listen 8080, (err) ->
  console.error err if err
  console.log 'listening at http://localhost:8080'
