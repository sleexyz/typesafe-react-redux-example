webpack = require 'webpack'
config = require './webpack'


webpack config, (err, stats) ->
  console.error err if err?
