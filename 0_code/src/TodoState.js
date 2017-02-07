// @flow
import {createStore, combineReducers} from 'redux'
import makeState from './makeState.js'

const initialState = {
  todos: [
    {value: 'write todomvc'},
    {value: 'enjoy how meta this is'},
  ]
};

const actionsObj = {
  addTodo(state, value: string) {
    return state;
  },
  removeTodo(state, index: number) {
    return state;
  }
}

export default makeState(initialState, actionsObj);
