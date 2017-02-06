// @flow

import React, {createElement as e} from 'react';
import ReactDOM from 'react-dom';
import App from './components/App'


const renderApp = () => {
  ReactDOM.render(
    <App/>,
    document.getElementById('root'),
  );
};

renderApp();

if (module.hot) {
  (module.hot: any).accept(undefined, renderApp);
}
