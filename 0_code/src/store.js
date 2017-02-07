// @flow
import {createStore, combineReducers} from 'redux'
import {reducer as TodoReducer} from './storeDefs/Todo.js'

const Todos = createStore(TodoReducer);

export default Todos;

if (module.hot) {
  (module.hot: any).accept();
}
