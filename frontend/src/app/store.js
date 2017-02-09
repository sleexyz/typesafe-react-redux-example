// @flow
import { applyMiddleware, createStore } from 'redux';
import createLogger from 'redux-logger';
import { reducer as TodoReducer } from 'app/store_defs/Todo';

const logger = createLogger({
  timestamp: false,
  collapsed: true,
  diff: true,
});

const Todos = createStore(
  TodoReducer,
  applyMiddleware(
    logger,
  ),
);

export default Todos;

if (module.hot) {
  // eslint-disable-next-line flowtype/no-weak-types
  (module.hot: any).accept();
}
