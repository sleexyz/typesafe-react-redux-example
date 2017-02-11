// @flow
import React from 'react';
import { dispatch } from 'app/redux';
import { actions } from 'app/state';
import type { Entry } from 'app/state/Todo';

const editTodo = (index) => (e) => {
  const value = e.target.value;
  dispatch(actions.Todo.updateTodo({ index, value }));
  dispatch(actions.Todo.updateTodo({ index, value }));
};

const closeTodo = (index) => () => {
  dispatch(actions.Todo.removeTodo(index));
};

const TodoListEntry = ({ value, index }: Entry & { index: number }) => (
  <div>
    <button onClick={closeTodo(index)}>(x)</button>
    <input value={value} onChange={editTodo(index)} />
  </div>
);

export default TodoListEntry;
