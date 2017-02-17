# Typesafe React-Redux Example [![CircleCI](https://circleci.com/gh/sleexyz/typesafe-react-redux-example.svg?style=svg)](https://circleci.com/gh/sleexyz/typesafe-react-redux-example)


## Features
- type safety + plenty of type inference via [flow](https://flowtype.org/)
- integration testing via [enzyme](https://github.com/airbnb/enzyme)
- typesafe, testable effects via [redux-ship](https://github.com/airbnb/enzyme)

## Libraries used

### Language
#### flowtype
- JS static analyzer + gradual type system

#### flow-typed
- repository of type definitions for untyped 3rd party libs

#### eslint
- standard js linter

#### eslint-config-airbnb
- popular js style guide

#### babel
- js compiler

### Build

#### webpack
- highly configurable module bundler

### View
#### react
- component-based view library

#### [styled-components](https://github.com/styled-components/styled-components)
- colocated *per-component* styles via es6 tagged template literals


### Effect System

#### [redux-ship](https://github.com/clarus/redux-ship)
- testable, typesafe async effect system

### Testing

#### mocha
- test runner

#### mocha-loader
- OOTB packaged with sleek mocha BDD UI
- hot-reloads all tests which have changed or depend transitively on files that have changed

#### source-map-support
- adds babel sourcemap support

#### chai
- assertion library
