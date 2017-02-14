// @flow
import React from 'react';
import TodoList from 'app/components/TodoList';
import styled from 'styled-components';
import { colors } from 'app/styles';

const AppContainer = styled.div`
height: 100vh;
width: 100vw;
display: flex;
flex-direction: column;
justify-content: center;
align-items: center;
filter: drop-shadow(.25rem .25rem 1.75rem ${colors.blue});
`;

const AppInner = styled.div`
width: 30rem;
`;

const Content = styled.div`
overflow-y: auto;
max-height: 75vh;
&::-webkit-scrollbar {
  display: none;
}
width: 100%;
`;

const App = () => (
  <AppContainer>
    <AppInner>
      <Content>
        <TodoList />
      </Content>
    </AppInner>
  </AppContainer>
);

export default App;
