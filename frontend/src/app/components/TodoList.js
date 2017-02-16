// @flow
import React from 'react';
import { connect } from 'react-redux';
import type { State } from 'app/model/Todo';
import { TodoLens } from 'app/model/Todo';
import TodoListEntry from 'app/components/TodoListEntry';

const TodoList = ({ todos }: State) => {
  return (
    <div>
      {todos.map((data, i) => <TodoListEntry {...data} index={i} key={data.id} />)}
    </div>
  );
};

export default connect(TodoLens.view)(TodoList);
