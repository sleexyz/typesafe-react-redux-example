#!/bin/sh
set -e
cd "$(dirname "$0")"
hlint --hint=app.hlint .
