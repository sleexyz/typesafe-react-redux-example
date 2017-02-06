// @flow
import React, {createElement as e} from 'react';

const inputOnChange = (ev) => {
  console.log(ev.target.value);
};

const TodoListEntry = ({value}: {value: string}) => {
  return (
    <div>
      <a>(x)</a>
      <input value={value} onChange={inputOnChange}/>
    </div>
  );
};

export default TodoListEntry;
