# Typesafe React-Redux Example

## Key Concepts

### Staged Compilation

At its heart, [Webpack](https://webpack.js.org/) is a build tool that takes in *code* and returns *code*.

In general we can have multiple compilation steps that take *code* and return *code*. A prototypical example of this is when the backend generates a computer-parseable API specification.

We can model these compilation steps as a graph. For example, this example project has multiple webpack outputs:

```
              ...

               |
               V

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
1. How state is structured.
2. The methods that modify those actions

A `Map<String, StoreDef>` can be transformed into a single, auto-namespaced StoreDef. (TODO: implement and document)

For those familiar with Redux, a StoreDef subsumes:
1. your Redux Store type
2. your Redux Store initial state
3. your Redux Reducers
4. your Redux Actions
5. your Redux Action Constants
6. A simple, synchronous subset of your Redux Action Creators
