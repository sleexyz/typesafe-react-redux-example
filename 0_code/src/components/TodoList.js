// @flow
import React, {createElement as e} from 'react';
import TodoListEntry from './TodoListEntry.js';
import store from '../store';
import TodoState from '../TodoState';

const TodoStateActions = TodoState.actions;

const addOnClick = () => {
  store.dispatch(TodoStateActions.addTodo());
};

const TodoList = () => {
  return (
    <div>
      <h2>TodoList</h2>
      {store.getState().todos.map((data, i) => <TodoListEntry {...data} key={i}/>)}
      <button onClick={addOnClick}>Add</button>
    </div>
  );
};

export default TodoList;
