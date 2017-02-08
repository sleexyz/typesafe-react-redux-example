// @flow
import {applyMiddleware, createStore, combineReducers} from 'redux';
import createLogger from 'redux-logger';
import {reducer as TodoReducer} from './storeDefs/Todo.js';

const logger = createLogger();
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
