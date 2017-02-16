#!/bin/sh

ghcid \
  -W \
  -c 'stack repl app:lib app:spec app:server --main-is app:server' \
  -T ':cmd return "let main = Main.main \n Spec.main \n :main"'
