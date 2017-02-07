// @flow
import {createStore, combineReducers} from 'redux'
import TodoState from './TodoState.js'

const Todos = createStore(TodoState.reducer);

export default Todos;

if (module.hot) {
  (module.hot: any).accept();
}
