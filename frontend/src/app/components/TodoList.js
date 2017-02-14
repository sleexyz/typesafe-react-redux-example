// @flow
import React from 'react';
import { connect, dispatch } from 'app/redux';
import type { State } from 'app/state/Todo';
import { actions, selectors } from 'app/state/Todo';
import TodoListEntry from 'app/components/TodoListEntry';

const addOnClick = () => {
  dispatch(actions.createTodo());
};

const TodoList = ({ todos }: State) => (
  <div>
    <h2>TodoList</h2>
    <button onClick={addOnClick} >Add</button>
    {todos.map((data, i) => <TodoListEntry {...data} index={i} key={data.id} />)}
  </div>
);

export default connect(selectors.main)(TodoList);
