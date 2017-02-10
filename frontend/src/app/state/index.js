// @flow
import { combineStateDefs } from 'state-def';
import Todo from './Todo';

const stateDef = combineStateDefs({
  Todo,
});

export const { actions, reducer } = stateDef;
