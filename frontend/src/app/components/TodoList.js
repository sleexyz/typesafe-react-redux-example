// @flow
import React from 'react';
import { connect } from 'react-redux';
import store from 'app/store';
import { actions as TodoActions } from 'app/store_defs/Todo';
import TodoListEntry from 'app/components/TodoListEntry';

const addOnClick = () => {
  store.dispatch(TodoActions.createTodo());
};

type Props = {
  todos: Array<{
    value: string,
    id: number
  }>
};

const TodoList = ({ todos }: Props) => (
  <div>
    <h2>TodoList</h2>
    <button onClick={addOnClick}>Add</button>
    {todos.map((data, i) => <TodoListEntry {...data} index={i} key={data.id} />)}
  </div>
);

const mapStateToProps = ({ todos }) => ({ todos });
export default connect(mapStateToProps)(TodoList);
