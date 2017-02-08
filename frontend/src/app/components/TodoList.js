// @flow
import React, {createElement as e} from 'react';
import {connect} from 'react-redux';
import store from 'app/store.js';
import {actions as TodoActions} from 'app/store_defs/Todo.js';
import TodoListEntry from 'app/components/TodoListEntry.js';

const addOnClick = () => {
  store.dispatch(TodoActions.createTodo());
};

const TodoList = ({todos}) =>
  <div>
    <h2>TodoList</h2>
    <button onClick={addOnClick}>Add</button>
    {todos.map((data, i) => <TodoListEntry {...data} index={i} key={data.id}/>)}
  </div>

const mapStateToProps = ({todos}) => ({todos});
export default connect(mapStateToProps)(TodoList);
