// @flow
import React, { Component } from 'react';
import styled from 'styled-components';
import { connect } from 'react-redux';
import { commits } from 'app/model/Todo';
import type { Entry } from 'app/model/Todo';
import { Button, colors } from 'app/styles';

export const Close = styled.button`
${Button()}
`;

const Row = styled.div`
display: flex;
margin: 0.25rem 0;
`;

export const Input = styled.input`
border: none;
border-bottom: 1px dashed ${colors.transgrey};
background: none;
width: auto;
flex: 1 0 auto;
`;

class TodoListEntry extends Component {
  props: Entry & { index: number } & { dispatch: * };

  editTodo = (e) => {
    const { dispatch } = this.props;
    const { index } = this.props;
    const value = e.target.value;
    dispatch(commits.updateTodo({ index, value }));
    dispatch(commits.updateTodo({ index, value }));
  }

  closeTodo = () => {
    const { dispatch } = this.props;
    const { index } = this.props;
    dispatch(commits.removeTodo(index));
  }

  makeNewRow = (e) => {
    const { dispatch } = this.props;
    if (e.key === 'Enter') {
      dispatch(commits.createTodo());
    }
  }

  render() {
    const { value } = this.props;
    return (
      <Row>
        <Close onClick={this.closeTodo} >x</Close>
        <Input
          autoFocus
          value={value}
          onChange={this.editTodo}
          onKeyDown={this.makeNewRow}
        />
      </Row>
    );
  }
}

export default connect()(TodoListEntry);
