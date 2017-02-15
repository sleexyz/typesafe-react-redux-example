// @flow
import React, { Component } from 'react';
import { connect } from 'react-redux';
import type { State } from 'app/state/Todo';
import { TodoLens } from 'app/state/Todo';
import TodoListEntry from 'app/components/TodoListEntry';

class TodoList extends Component {
  props: State;

  render() {
    const { todos } = this.props;
    return (
      <div>
        {todos.map((data, i) => <TodoListEntry {...data} index={i} key={data.id} />)}
      </div>
    );
  }
}

export default connect(TodoLens.view)(TodoList);
