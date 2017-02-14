// @flow
import React from 'react';
import styled from 'styled-components';
import { dispatch } from 'app/redux';
import { actions } from 'app/state/Todo';
import type { Entry } from 'app/state/Todo';
import { Button, colors } from 'app/styles';

const Close = styled.button`
${Button()}
`;

const Row = styled.div`
display: flex;
margin: 0.25rem 0;
`;

const Input = styled.input`
border: none;
border-bottom: 1px dashed ${colors.transgrey};
background: none;
width: auto;
flex: 1 0 auto;
`;

const editTodo = (index) => (e) => {
  const value = e.target.value;
  dispatch(actions.updateTodo({ index, value }));
  dispatch(actions.updateTodo({ index, value }));
};

const closeTodo = (index) => () => {
  dispatch(actions.removeTodo(index));
};

const makeNewRow = (e) => {
  if (e.key === 'Enter') {
    dispatch(actions.createTodo());
  }
};

const TodoListEntry = ({ value, index }: Entry & { index: number }) => (
  <Row>
    <Close onClick={closeTodo(index)} >(x)</Close>
    <Input
      autoFocus
      value={value}
      onChange={editTodo(index)}
      onKeyDown={makeNewRow}
    />
  </Row>
);

export default TodoListEntry;
