// @flow
import {createStore, combineReducers} from 'redux'
import {makeStoreDef} from '../StoreDef.js'

const initialState = {
  todos: [
    {value: 'write todomvc'},
    {value: 'enjoy how meta this is'},
  ]
}

const actionsDef = (state) => ({
  addTodo(payload: typeof undefined) {
    return state;
  },
  removeTodo(value: number) {
    return state;
  }
});

const {actions, reducer} = makeStoreDef(initialState, actionsDef);
export {actions, reducer};
