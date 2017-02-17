// @flow
import React from 'react';
import ReactDOM from 'react-dom';
import { Provider } from 'react-redux';
import { applyMiddleware, createStore } from 'redux';
import createLogger from 'redux-logger';
import * as Ship from 'redux-ship';
import App from 'app/components/App';
import * as Model from 'app/model';
import * as Effects from 'app/effects';
import * as Controller from 'app/controllers';
import { logControl } from 'redux-ship-logger';

const store = createStore(Model.reducer, applyMiddleware(
  Ship.middleware(Effects.runEffects, Controller.runAsyncActions),
  createLogger({ timestamp: false, collapsed: true, diff: true }),
));

// $ExpectError
() => Ship.run(Effects.runEffects, store, logControl(Controller.runAsyncActions)(Controller.asyncActions.fo()));

Ship.run(Effects.runEffects, store, logControl(Controller.runAsyncActions)(Controller.asyncActions.foo()));

const renderApp = () => {
  ReactDOM.render(
    <Provider store={store}>
      <App />
    </Provider>,
    document.getElementById('root'),
  );
};

renderApp();

if (module.hot) {
  // eslint-disable-next-line flowtype/no-weak-types
  (module.hot: any).accept(undefined, renderApp);
}
