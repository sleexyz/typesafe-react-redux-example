// @flow
import { applyMiddleware, createStore } from 'redux';
import createLogger from 'redux-logger';
import { reducer } from 'app/store_defs';

const logger = createLogger({
  timestamp: false,
  collapsed: true,
  diff: true,
});

const Todos = createStore(
  reducer,
  applyMiddleware(
    logger,
  ),
);

export default Todos;

if (module.hot) {
  // eslint-disable-next-line flowtype/no-weak-types
  (module.hot: any).accept();
}
