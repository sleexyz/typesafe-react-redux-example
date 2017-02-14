# TodoMVC Backend

Built with [servant](http://haskell-servant.readthedocs.io/en/stable/)
and [servant-swagger](https://hackage.haskell.org/package/servant-swagger),
which generates a Swagger spec from a Servant API.

## Setup

Install [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/), the Haskell build tool:

Set up Stack:
```
stack update && stack setup
```

Install [ghcid](https://github.com/ndmitchell/ghcid), an autorecompiler:
```
stack install ghcid
```

Build for the first time:
```
stack build
```
## Develop

Running the following script will watch your Haskell files for changes. If a change is detected, it will recompile your code, rerun the test suite, and restart the mocked server.
```
./scripts/watch.sh
```

The mock server will be running on `localhost:8000`.

Visiting http://localhost:8000 in your browser will take you to a UI generated with [swagger-ui](https://github.com/swagger-api/swagger-ui).

`swagger.json` is located at `localhost:8080/swagger.json`
