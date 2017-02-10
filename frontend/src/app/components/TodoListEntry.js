// @flow
import React from 'react';
import store from 'app/store';
import { actions as Actions } from 'app/store_defs';

type Props = {
  value: string,
  index: number,
};

const editTodo = (index) => (e) => {
  const value = e.target.value;
  store.dispatch(Actions.Todo.updateTodo({ index, value }));
};

const closeTodo = (index) => () => {
  store.dispatch(Actions.Todo.removeTodo(index));
};

const TodoListEntry = ({ value, index }: Props) => (
  <div>
    <button onClick={closeTodo(index)}>(x)</button>
    <input value={value} onChange={editTodo(index)} />
  </div>
);

export default TodoListEntry;
