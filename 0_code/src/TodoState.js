// @flow
import {createStore, combineReducers} from 'redux'
import makeState from './makeState.js'

const initialState = {
  todos: [
    {value: 'write todomvc'},
    {value: 'enjoy how meta this is'},
  ]
};

const generateActions = (state) => ({
  addTodo() {
    console.log('adding todo...');
    return state;
  },
  removeTodo(value: number) {
    return state;
  }
});

export default makeState(initialState, generateActions);
