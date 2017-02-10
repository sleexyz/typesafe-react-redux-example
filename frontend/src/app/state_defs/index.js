// @flow
import { combineStoreDefs } from 'store-def';
import Todo from './Todo';

const storeDef = combineStoreDefs({
  Todo,
});

export const { actions, reducer } = storeDef;
