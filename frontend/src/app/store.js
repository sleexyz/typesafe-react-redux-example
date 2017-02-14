// @flow
import { applyMiddleware, createStore } from 'redux';
import { ReducerBuilder } from 'state-def';
import createLogger from 'redux-logger';
import { stateDef as Todo } from 'app/state/Todo';

const reducer = ReducerBuilder
  .init()
  .use(Todo)
  .toReducer();

const logger = createLogger({
  timestamp: false,
  collapsed: true,
  diff: true,
});

const store = createStore(
  reducer,
  applyMiddleware(
    logger,
  ),
);

export default store;

if (module.hot) {
  // eslint-disable-next-line flowtype/no-weak-types
  (module.hot: any).accept();
}
