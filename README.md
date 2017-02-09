# Typesafe React-Redux Example
[![CircleCI](https://circleci.com/gh/sleexyz/typesafe-react-redux-example.svg?style=svg)](https://circleci.com/gh/sleexyz/typesafe-react-redux-example)


## Key Concepts

### Embrace Staged Compilation

At its heart, [Webpack](https://webpack.js.org/) is a build tool that takes in *code* and returns *code*.

In general we can have multiple compilation steps that *code* and return *code*. A prototypical example of this is when some backend script generates a computer-parseable API specification. If we wanted to, we could theoretically generate client-side service code from this specification. (TODO: implement)

We can model these compilation steps as a graph. For example, this example project has multiple webpack outputs, modelled by the following diagram:

```

            frontend

     /         |           \
    V          V            V

dist_app  dist_test_code  dist_test_app

```

In this example project, we choose the convention of placing all generated code in `./FOO/src/gen`, as this simplified directory tree demonstrates:


```
.
├── frontend
│   └── src
│       └── gen/
│
├── dist_app
│   └── src
│      └── gen/
│
├── dist_test_node
│   └── src
│      └── gen/
│
└── dist_test_web
    └── src
       └── gen/
```

### StoreDefs

StoreDefs are units of data that describe:

1. how state is structured
2. how state is initialized
3. the methods that modify those actions

An object with StoreDef values can be transformed into a single, auto-namespaced StoreDef. (TODO: implement and document)

For those familiar with Redux, a StoreDef is a unit of data that subsumes:

1. your Redux Store type
2. your Redux Store initial state
3. your Redux Reducers
4. your Redux Actions
5. your Redux Action Constants
6. A simple, synchronous subset of your Redux Action Creators
