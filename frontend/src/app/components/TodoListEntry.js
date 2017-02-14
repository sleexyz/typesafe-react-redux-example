// @flow
import React from 'react';
import { dispatch } from 'app/redux';
import { actions } from 'app/state/Todo';
import type { Entry } from 'app/state/Todo';

const editTodo = (index) => (e) => {
  const value = e.target.value;
  dispatch(actions.updateTodo({ index, value }));
  dispatch(actions.updateTodo({ index, value }));
};

const closeTodo = (index) => () => {
  dispatch(actions.removeTodo(index));
};

const TodoListEntry = ({ value, index }: Entry & { index: number }) => (
  <div>
    <button onClick={closeTodo(index)}>(x)</button>
    <input value={value} onChange={editTodo(index)} />
  </div>
);

export default TodoListEntry;
