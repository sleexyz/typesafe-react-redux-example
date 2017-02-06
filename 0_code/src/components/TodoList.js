// @flow
import React, {createElement as e} from 'react';
import TodoListEntry from './TodoListEntry.js';
import store from '../store'

const addOnClick = () => {
  console.log('Add');
};

const TodoList = () => {
  console.log(store);
  return (
    <div>
      <h2>TodoList</h2>
      {[].map((data, i) => <TodoListEntry {...data} key={i}/>)}
      <button onClick={addOnClick}>Add</button>
    </div>
  );
};

export default TodoList;
