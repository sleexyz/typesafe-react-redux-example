// @flow
import React from 'react';
import ReactDOM from 'react-dom';
import { Provider } from 'react-redux';
import App from 'app/components/App';
import store from 'app/store';

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
