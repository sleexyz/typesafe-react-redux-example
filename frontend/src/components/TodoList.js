// @flow
import React, {createElement as e} from 'react';
import {connect} from 'react-redux';
import TodoListEntry from './TodoListEntry.js';
import store from '../store';
import {actions as TodoActions} from '../storeDefs/Todo.js';

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
