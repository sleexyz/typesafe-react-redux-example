import { combineStateDefs } from 'state-def';
import Todo from './Todo';

const stateDef = combineStateDefs({
  Todo,
});

export type State = typeof stateDef.initialState;
export const { actions, reducer, selectors } = stateDef;
