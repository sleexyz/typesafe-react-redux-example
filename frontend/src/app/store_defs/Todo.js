// @flow
import {createStore, combineReducers} from 'redux';
import {makeStoreDef} from 'store-def';

const initialState = {
  nextId: 1,
  todos: [
    {value: 'write todomvc', id: 0},
  ],
};

const reducerMap = {
  createTodo(state) {
    return {
      nextId: state.nextId + 1,
      todos: [
        ...state.todos,
        {value: '', id: state.nextId},
      ],
    };
  },
  removeTodo(state, index: number) {
    const newTodos = [...state.todos];
    newTodos.splice(index, 1);
    return {
      ...state,
      todos: newTodos,
    };
  },
  updateTodo(state, {index, value}: {index: number, value: string}) {
    const newTodos = [...state.todos];
    const oldTodo = state.todos[index];
    newTodos.splice(index, 1, {...oldTodo, value});
    return {
      ...state,
      todos: newTodos,
    };
  }
};

const {actions, reducer} = makeStoreDef(initialState, reducerMap);
export {actions, reducer};
