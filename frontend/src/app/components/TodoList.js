// @flow
import React from 'react';
import { connect } from 'app/redux';
import type { State } from 'app/state/Todo';
import { TodoLens } from 'app/state/Todo';
import TodoListEntry from 'app/components/TodoListEntry';

const TodoList = ({ todos }: State) => (
  <div>
    {todos.map((data, i) => <TodoListEntry {...data} index={i} key={data.id} />)}
  </div>
);

export default connect(TodoLens.view)(TodoList);
