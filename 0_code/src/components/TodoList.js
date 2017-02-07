// @flow
import React, {createElement as e} from 'react';
import TodoListEntry from './TodoListEntry.js';
import store from '../store';
import {actions as TodoActions} from '../storeDefs/Todo.js';

const addOnClick = () => {
  store.dispatch(TodoActions.addTodo());
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
