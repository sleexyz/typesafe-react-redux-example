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

### StateDefs

StateDefs are units of data that describe:

1. the types of state
2. how state is initialized
3. elementary, synchronous methods that modify those actions

An object with StateDef values can be transformed into a single, StateDefCollection with `combineStateDefs`. (TODO: document once API is settled down)

For those familiar with Redux, a StateDef is a unit of data that subsumes:

1. Redux Store Types
2. Redux Store initial state
3. Redux Reducers
4. Redux Actions
5. Redux Action Constants
6. An elementary, synchronous subset of Redux Action Creators
7. Redux Selectors (TODO: implement)

### Other Architectural Decisions

#### 1) Import `dispatch` directly
Many of Redux + React-Redux's design decisions stem from the requirement of server-side rendering.
For example, that's why [`dispatch` is always inject via props](http://stackoverflow.com/questions/33221634/why-use-this-props-dispatch-rather-than-store-dispatch-directly-in-redux).
If we're never going to do server rendering, then we shouldn't make sacrifices for it.

Note: we still use `Provider` and `connect` to make components reactive to changes to the store.

#### 2) Unconstrained Component Hierarchy
We should expand our palette of ultra-dumb, reusable components as much we can.

But there's no benefit to putting a constraint on the structure of component *hierarchies*; namely, a constraint that only top-level components should be connected to Redux.
Constraining state-awareness to top-level components is unscalable for complex UI's with multiple *intrinsically* state-aware parts.
We want to be as close to the store as we can and minimize the amount of props we need to pass around, by emphasizing *selectors* over manual *state* injection via props.
