// @flow
import React from 'react';
import { connect } from 'react-redux';
import store from 'app/store';
import { actions } from 'app/state';
import TodoListEntry from 'app/components/TodoListEntry';

const addOnClick = () => {
  store.dispatch(actions.Todo.createTodo());
};

type Props = {
  todos: Array<{
    value: string,
    id: number
  }>,
};

const TodoList = ({ todos }: Props) => (
  <div>
    <h2>TodoList</h2>
    <button onClick={addOnClick}>Add</button>
    {todos.map((data, i) => <TodoListEntry {...data} index={i} key={data.id} />)}
  </div>
);

export default connect(({ Todo }) => Todo)(TodoList);
