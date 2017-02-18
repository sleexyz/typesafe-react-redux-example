# Typesafe React + Redux Example [![CircleCI](https://circleci.com/gh/sleexyz/typesafe-react-redux-example.svg?style=svg)](https://circleci.com/gh/sleexyz/typesafe-react-redux-example)


## Features
- Static analysis + type safety with [Flow](https://flowtype.org/)
- Full spectrum application via [Enzyme](https://github.com/airbnb/enzyme)
- Controlled effect system via [redux-ship](https://github.com/clarus/redux-ship)
- Type-checked style variables via [styled-components](https://github.com/styled-components/styled-components)
- **TODO** Derived type definitions from API specifications via [json-to-flow](https://github.com/STRML/json-to-flow)

## Main ideas

### Redux, Deboilerplated
In its standard form, Redux requires inane amounts of boilerplate by encouraging manual splitting of "action creators", "actions", "action constants", and "reducers".

Instead, we define maps of parametrized state transitions (think reducer branches), and then combine them in such a way that derives all the necessary Redux machinery.

### Controlled effects via [Redux Ship](https://github.com/clarus/redux-ship)
With vanilla Redux, we have two application-specific parameters:
- The definition of your **state**
- A set of synchronous state transitions, traditionally called "actions", hereon called **commits**

Redux-ship adds two more:
- A set of asynchronous effects, hereon called **effects**
- A set of asynchronous computations, which include execution of effects or issue commits to the state, hereon called **async actions** or simply **actions**

We use the same map encoding to reduce boilerplate for defining these effects and actions.

### Type annotations where they count
Flow's strength lies in its type inference. This allows us to have static guarantees about our code, without having to pollute our codebase with type annotations everywhere.

We write types where they count, and allow the rest to be inferred. This means writing:
- types of the state, and any function that act on the state.
- types of async actions
- prop types

## Libraries used

### Language

#### [flow](https://flowtype.org/)
- Flow is a type checker for JS.

#### [flow-typed](https://github.com/flowtype/flow-typed)
- flow-typed is a repository of type definitions for 3rd party libraries.

#### eslint
- eslint is a JS linter.
- Our style is based on [eslint-config-airbnb](https://github.com/airbnb/javascript).

#### Babel
- Babel is a JS compiler.
- Among standard ES6 features, we also use
  - [Class property initializers](https://babeljs.io/docs/plugins/transform-class-properties/), which solve the problem of binding React class methods
  - [Object rest spread](https://babeljs.io/docs/plugins/transform-object-rest-spread/)

### Build

#### [Webpack](https://webpack.js.org/)
- Webpack is a highly configurable JS module bundler.
- It provides a hot-module replacement API, which we use in development mode without the use of react-hot-loader.

### View
#### [React](https://facebook.github.io/react/)
- React is a UI framework.

#### [styled-components](https://github.com/styled-components/styled-components)
- styled-components give us colocated, *per-component* styling via es6 tagged template literals
- We also get type-checked styling variables by virtue of writing styles in JS + Flow.

### State Management

#### [Redux](http://redux.js.org/)
- Redux is a low-level API for building state management systems.

### Effect System

#### [redux-ship](https://github.com/clarus/redux-ship)
- redux-ship is a controlled effect system for JS, which plays well with the redux ecosystem.
- It gives us testable, typesafe asynchronous computations.

### Testing

#### [mocha](https://mochajs.org/) 
- Mocha is a test runner.

#### [mocha-loader](https://github.com/webpack-contrib/mocha-loader)
- mocha-loader is a webpack loader that compiles mocha tests suites.
- We use mocha-loader to run JS tests in CI.
- We also use mocha-loader to run JS tests locally in the browser, which includes a sleek, hot-reloading test reporter.
- It will hot-reload all tests which have changed or depend transitively on files that have changed.

#### [enzyme](https://github.com/airbnb/enzyme)
- Enzyme is a suite of React component testing utilities.
- We use it for unit, integration, and feature tests.
