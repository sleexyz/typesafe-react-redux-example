// @flow
import React from 'react';
import ReactDOM from 'react-dom';
import { Provider } from 'react-redux';
import { applyMiddleware, createStore } from 'redux';
import createLogger from 'redux-logger';
import App from 'app/components/App';
import reducer from 'app/reducer';
import 'app/effects';

const store = createStore(reducer, applyMiddleware(
  createLogger({ timestamp: false, collapsed: true, diff: true }),
));

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
