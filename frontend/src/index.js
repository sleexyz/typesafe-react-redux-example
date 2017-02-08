// @flow
import React, {createElement as e} from 'react';
import ReactDOM from 'react-dom';
import {Provider} from 'react-redux';
import App from './components/App'
import store from './store';

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
