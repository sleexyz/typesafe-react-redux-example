// @flow
import React, {createElement as e} from 'react';
import ReactDOM from 'react-dom';
import {Provider} from 'react-redux';
import App from 'app/components/App.js'
import store from 'app/store.js';

const renderApp = () => {
  ReactDOM.render(
    <Provider store={store}>
      <App/>
    </Provider>,
    document.getElementById('root'),
  );
};

renderApp();

if (module.hot) {
  (module.hot: any).accept(undefined, renderApp);
}
