// @flow
import {applyMiddleware, createStore, combineReducers} from 'redux';
import createLogger from 'redux-logger';
import {reducer as TodoReducer} from 'app/store_defs/Todo.js';

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
  (module.hot: any).accept();
}
