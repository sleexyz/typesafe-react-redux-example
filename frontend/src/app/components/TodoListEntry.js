// @flow
import React from 'react';
import store from 'app/store';
import { actions as TodoActions } from 'app/store_defs/Todo';

type Props = {
  value: string,
  index: number,
};

const editTodo = (index) => (e) => {
  const value = e.target.value;
  store.dispatch(TodoActions.updateTodo({ index, value }));
};

const closeTodo = (index) => () => {
  store.dispatch(TodoActions.removeTodo(index));
};

const TodoListEntry = ({ value, index }: Props) => (
  <div>
    <button onClick={closeTodo(index)}>(x)</button>
    <input value={value} onChange={editTodo(index)} />
  </div>
);

export default TodoListEntry;
