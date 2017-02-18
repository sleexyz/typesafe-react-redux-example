// @flow
import React from 'react';
import ReactDOM from 'react-dom';
import { Provider } from 'react-redux';
import { applyMiddleware, createStore } from 'redux';
import createLogger from 'redux-logger';
import * as ShipDevTools from 'redux-ship-devtools';
import * as Ship from 'redux-ship';
import * as ShipLogger from 'redux-ship-logger';

import App from 'app/components/App';
import * as Model from 'app/model';
import * as Effects from 'app/effects';
import * as Actions from 'app/actions';
import * as TodoActions from 'app/actions/TodoActions';

const store = createStore(Model.reducer, applyMiddleware(
  Ship.middleware(Effects.run, Actions.run),
  createLogger({ timestamp: false, collapsed: true, diff: true }),
));

const runActions = ShipDevTools.inspect(ShipLogger.logControl(Actions.run));

Ship.run(Effects.run, store, runActions(TodoActions.actions.displayWelcome()));

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
