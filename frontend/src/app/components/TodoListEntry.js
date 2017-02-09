// @flow
import React from 'react';
import store from 'app/store.js';
import {actions as TodoActions} from 'app/store_defs/Todo.js';

type Props = {
  value: string,
  index: number,
};

const editTodo = (index) => (e) => {
  const value = e.target.value;
  store.dispatch(TodoActions.updateTodo({index, value}));
};

const closeTodo = (index) => () => {
  store.dispatch(TodoActions.removeTodo(index))
};

const TodoListEntry = ({value, index}: Props) => {
  return (
    <div>
      <a onClick={closeTodo(index)}>(x)</a>
      <input value={value} onChange={editTodo(index)}/>
    </div>
  );
};

export default TodoListEntry;
